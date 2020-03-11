package timeusage

import org.apache.spark.sql._
import org.apache.spark.sql.types._

/** Main class */
object TimeUsage extends TimeUsageInterface {

  import org.apache.spark.sql.SparkSession
  import org.apache.spark.sql.functions._

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Time Usage")
      .master("local")
      .getOrCreate()

  // For implicit conversions like converting RDDs to DataFrames
  import spark.implicits._

  /** Main function */
  def main(args: Array[String]): Unit = {
    timeUsageByLifePeriod()
    spark.close()
  }

  def timeUsageByLifePeriod(): Unit = {
    val (columns, initDf) = read("src/main/resources/timeusage/atussum.csv")
    val (primaryNeedsColumns, workColumns, otherColumns) = classifiedColumns(columns)
    val summaryDf = timeUsageSummary(primaryNeedsColumns, workColumns, otherColumns, initDf)
    val finalDf = timeUsageGrouped(summaryDf)
    finalDf.show()
  }

  /** @return The read DataFrame along with its column names. */
  def read(path: String): (List[String], DataFrame) = {
    val df = spark.read.options(Map("header" -> "true", "inferSchema" -> "true")).csv(path)
    (df.schema.fields.map(_.name).toList, df)
  }

  /** @return An RDD Row compatible with the schema produced by `dfSchema`
    * @param line Raw fields
    */
  def row(line: List[String]): Row = {
    Row.fromSeq(line.head :: line.tail.map(_.toDouble))
  }

  /** @return The initial data frame columns partitioned in three groups: primary needs (sleeping, eating, etc.),
    *         work and other (leisure activities)
    *
    * @see https://www.kaggle.com/bls/american-time-use-survey
    *
    * The dataset contains the daily time (in minutes) people spent in various activities. For instance, the column
    * “t010101” contains the time spent sleeping, the column “t110101” contains the time spent eating and drinking, etc.
    *
    * This method groups related columns together:
    * 1. “primary needs” activities (sleeping, eating, etc.). These are the columns starting with “t01”, “t03”, “t11”,
    *    “t1801” and “t1803”.
    * 2. working activities. These are the columns starting with “t05” and “t1805”.
    * 3. other activities (leisure). These are the columns starting with “t02”, “t04”, “t06”, “t07”, “t08”, “t09”,
    *    “t10”, “t12”, “t13”, “t14”, “t15”, “t16” and “t18” (those which are not part of the previous groups only).
    */
  def classifiedColumns(columnNames: List[String]): (List[Column], List[Column], List[Column]) = {
    val nonValueColumns = List("tucaseid", "gemetsta", "gtmetsta", "peeduca", "pehspnon", "ptdtrace", "teage", "telfs", "temjot", "teschenr", "teschlvl", "tesex", "tespempnot", "trchildnum", "trdpftpt", "trernwa", "trholiday", "trspftpt", "trsppres", "tryhhchild", "tudiaryday", "tufnwgtp", "tehruslt", "tuyear")
    val valueColumns = columnNames.filter(name => !nonValueColumns.contains(name))
    // Set(t1112, t180101, t0102, t0301, t010101, t180301)
    val primaryNeedsColumns =
      valueColumns
        .filter(name =>
          name.startsWith("t01") ||
            name.startsWith("t03") ||
            name.startsWith("t11") ||
            name.startsWith("t1801") ||
            name.startsWith("t1803"))
        .map(name => col(name)) // .map(col _)
    val workColumns =
      valueColumns
        .filter(name =>
          name.startsWith("t05") ||
            name.startsWith("t1805"))
        .map(name => col(name))
    val otherColumns =
      valueColumns
        .filter(name => (!primaryNeedsColumns.contains(col(name)) && !workColumns.contains(col(name))))
        .filter(name => name.startsWith("t02") ||
          name.startsWith("t04") ||
          name.startsWith("t06") ||
          name.startsWith("t07") ||
          name.startsWith("t08") ||
          name.startsWith("t09") ||
          name.startsWith("t10") ||
          name.startsWith("t12") ||
          name.startsWith("t13") ||
          name.startsWith("t14") ||
          name.startsWith("t15") ||
          name.startsWith("t16") ||
          name.startsWith("t18"))
        .map(name => col(name))
    (primaryNeedsColumns, workColumns, otherColumns)
  }

  /** @return a projection of the initial DataFrame such that all columns containing hours spent on primary needs
    *         are summed together in a single column (and same for work and leisure). The “teage” column is also
    *         projected to three values: "young", "active", "elder".
    *
    * @param primaryNeedsColumns List of columns containing time spent on “primary needs”
    * @param workColumns List of columns containing time spent working
    * @param otherColumns List of columns containing time spent doing other activities
    * @param df DataFrame whose schema matches the given column lists
    *
    * This methods builds an intermediate DataFrame that sums up all the columns of each group of activity into
    * a single column.
    *
    * The resulting DataFrame should have the following columns:
    * - working: value computed from the “telfs” column of the given DataFrame:
    *   - "working" if 1 <= telfs < 3
    *   - "not working" otherwise
    * - sex: value computed from the “tesex” column of the given DataFrame:
    *   - "male" if tesex = 1, "female" otherwise
    * - age: value computed from the “teage” column of the given DataFrame:
    *   - "young" if 15 <= teage <= 22,
    *   - "active" if 23 <= teage <= 55,
    *   - "elder" otherwise
    * - primaryNeeds: sum of all the `primaryNeedsColumns`, in hours
    * - work: sum of all the `workColumns`, in hours
    * - other: sum of all the `otherColumns`, in hours
    *
    * Finally, the resulting DataFrame should exclude people that are not employable (ie telfs = 5).
    *
    * Note that the initial DataFrame contains time in ''minutes''. You have to convert it into ''hours''.
    */
  def timeUsageSummary(
    primaryNeedsColumns: List[Column],
    workColumns: List[Column],
    otherColumns: List[Column],
    df: DataFrame
  ): DataFrame = {
    // Transform the data from the initial dataset into data that make
    // more sense for our use case
    // Hint: you can use the `when` and `otherwise` Spark functions
    // Hint: don’t forget to give your columns the expected name with the `as` method
    val workingStatusProjection: Column =
    when($"telfs" >= 1 && $"telfs" < 3, "working")
      .otherwise("not working")
      .as("working")

    val sexProjection: Column =
      when($"tesex" === 1, "male")
        .otherwise("female")
        .as("sex")

    val ageprj = udf ( (d:Double) =>
      if (d >= 15 && d <=22 ) "young"
      else if (d >= 23 && d <= 55) "active"
      else "elder")
    val ageProjection: Column =
      ageprj(col("teage"))
        .as("age")

    def sum_(cols: Column*) = cols.reduce(_ + _) / 60

    // Create columns that sum columns of the initial dataset
    // Hint: you want to create a complex column expression that sums other columns
    //       by using the `+` operator between them
    // Hint: don’t forget to convert the value to hours
    val primaryNeedsProjection: Column =
    sum_(primaryNeedsColumns: _*)
      .as("primaryNeedsProjection")

    val workProjection: Column =
      sum_(workColumns: _*)
        .as("workProjection")

    val otherProjection: Column =
      sum_(otherColumns: _*)
        .as("otherProjection")

    df.select(workingStatusProjection, sexProjection, ageProjection, primaryNeedsProjection, workProjection, otherProjection)
      .where($"telfs" <= 4) // Discard people who are not in labor force
  }

  /** @return the average daily time (in hours) spent in primary needs, working or leisure, grouped by the different
    *         ages of life (young, active or elder), sex and working status.
    * @param summed DataFrame returned by `timeUsageSumByClass`
    *
    * The resulting DataFrame should have the following columns:
    * - working: the “working” column of the `summed` DataFrame,
    * - sex: the “sex” column of the `summed` DataFrame,
    * - age: the “age” column of the `summed` DataFrame,
    * - primaryNeeds: the average value of the “primaryNeeds” columns of all the people that have the same working
    *   status, sex and age, rounded with a scale of 1 (using the `round` function),
    * - work: the average value of the “work” columns of all the people that have the same working status, sex
    *   and age, rounded with a scale of 1 (using the `round` function),
    * - other: the average value of the “other” columns all the people that have the same working status, sex and
    *   age, rounded with a scale of 1 (using the `round` function).
    *
    * Finally, the resulting DataFrame should be sorted by working status, sex and age.
    */
  def timeUsageGrouped(summed: DataFrame): DataFrame = {
    summed
      .groupBy($"working", $"sex", $"age")
      .agg(
        avg($"primaryNeedsProjection").as("primaryNeeds"),
        avg($"workProjection").as("work"),
        avg($"otherProjection").as("other"))
      .withColumn("primaryNeeds", round($"primaryNeeds", 1))
      .withColumn("work", round($"work", 1))
      .withColumn("other", round($"other", 1))
      .orderBy($"working", $"sex", $"age")
  }

  /**
    * @return Same as `timeUsageGrouped`, but using a plain SQL query instead
    * @param summed DataFrame returned by `timeUsageSumByClass`
    */
  def timeUsageGroupedSql(summed: DataFrame): DataFrame = {
    val viewName = s"summed"
    summed.createOrReplaceTempView(viewName)
    spark.sql(timeUsageGroupedSqlQuery(viewName))
  }

  /** @return SQL query equivalent to the transformation implemented in `timeUsageGrouped`
    * @param viewName Name of the SQL view to use
    */
  def timeUsageGroupedSqlQuery(viewName: String): String =
    "SELECT working, sex, age, " +
      "ROUND(AVG(primaryNeedsProjection),1) as primaryNeeds, " +
      "ROUND(AVG(workProjection),1) as work, " +
      "ROUND(AVG(otherProjection),1) as other " +
      "FROM " + viewName + " GROUP BY working, sex, age " +
      "ORDER BY working, sex, age"

  /**
    * @return A `Dataset[TimeUsageRow]` from the “untyped” `DataFrame`
    * @param timeUsageSummaryDf `DataFrame` returned by the `timeUsageSummary` method
    *
    * Hint: you should use the `getAs` method of `Row` to look up columns and
    * cast them at the same time.
    */

  import spark.implicits._

  def timeUsageSummaryTyped(timeUsageSummaryDf: DataFrame): Dataset[TimeUsageRow] =
    timeUsageSummaryDf.map(xs =>
      TimeUsageRow(
        xs.getAs[String]("working"),
        xs.getAs[String]("sex"),
        xs.getAs[String]("age"),
        xs.getAs[Double]("primaryNeedsProjection"),
        xs.getAs[Double]("workProjection"),
        xs.getAs[Double]("otherProjection")
      ))

  /**
    * @return Same as `timeUsageGrouped`, but using the typed API when possible
    * @param summed Dataset returned by the `timeUsageSummaryTyped` method
    *
    * Note that, though they have the same type (`Dataset[TimeUsageRow]`), the input
    * dataset contains one element per respondent, whereas the resulting dataset
    * contains one element per group (whose time spent on each activity kind has
    * been aggregated).
    *
    * Hint: you should use the `groupByKey` and `typed.avg` methods.
    */
  def timeUsageGroupedTyped(summed: Dataset[TimeUsageRow]): Dataset[TimeUsageRow] = {
    import org.apache.spark.sql.expressions.scalalang.typed
    summed
      .groupByKey(xs => (xs.working, xs.sex, xs.age))
      .agg(typed.avg(_.primaryNeeds), typed.avg(_.work), typed.avg(_.other))
      //      .withColumn("primaryNeeds", round($"primaryNeeds", 1))
      //      .withColumn("work", round($"work", 1))
      //      .withColumn("other", round($"other", 1))
      .map(xs =>
      TimeUsageRow(xs._1._1, xs._1._2, xs._1._3,
        (xs._2 * 10.0).round / 10.0,
        (xs._3 * 10.0).round / 10.0,
        (xs._4 * 10.0).round / 10.0))
      .orderBy("working", "sex", "age")
  }
}

/**
  * Models a row of the summarized data set
  * @param working Working status (either "working" or "not working")
  * @param sex Sex (either "male" or "female")
  * @param age Age (either "young", "active" or "elder")
  * @param primaryNeeds Number of daily hours spent on primary needs
  * @param work Number of daily hours spent on work
  * @param other Number of daily hours spent on other activities
  */
case class TimeUsageRow(
  working: String,
  sex: String,
  age: String,
  primaryNeeds: Double,
  work: Double,
  other: Double
)
