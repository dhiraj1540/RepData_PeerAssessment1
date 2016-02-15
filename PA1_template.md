<!DOCTYPE html>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>

<title></title>

<script type="text/javascript">
window.onload = function() {
  var imgs = document.getElementsByTagName('img'), i, img;
  for (i = 0; i < imgs.length; i++) {
    img = imgs[i];
    // center an image if it is the only element of its parent
    if (img.parentElement.childElementCount === 1)
      img.parentElement.style.textAlign = 'center';
  }
};
</script>





<style type="text/css">
body, td {
   font-family: sans-serif;
   background-color: white;
   font-size: 13px;
}

body {
  max-width: 800px;
  margin: auto;
  padding: 1em;
  line-height: 20px;
}

tt, code, pre {
   font-family: 'DejaVu Sans Mono', 'Droid Sans Mono', 'Lucida Console', Consolas, Monaco, monospace;
}

h1 {
   font-size:2.2em;
}

h2 {
   font-size:1.8em;
}

h3 {
   font-size:1.4em;
}

h4 {
   font-size:1.0em;
}

h5 {
   font-size:0.9em;
}

h6 {
   font-size:0.8em;
}

a:visited {
   color: rgb(50%, 0%, 50%);
}

pre, img {
  max-width: 100%;
}
pre {
  overflow-x: auto;
}
pre code {
   display: block; padding: 0.5em;
}

code {
  font-size: 92%;
  border: 1px solid #ccc;
}

code[class] {
  background-color: #F8F8F8;
}

table, td, th {
  border: none;
}

blockquote {
   color:#666666;
   margin:0;
   padding-left: 1em;
   border-left: 0.5em #EEE solid;
}

hr {
   height: 0px;
   border-bottom: none;
   border-top-width: thin;
   border-top-style: dotted;
   border-top-color: #999999;
}

@media print {
   * {
      background: transparent !important;
      color: black !important;
      filter:none !important;
      -ms-filter: none !important;
   }

   body {
      font-size:12pt;
      max-width:100%;
   }

   a, a:visited {
      text-decoration: underline;
   }

   hr {
      visibility: hidden;
      page-break-before: always;
   }

   pre, blockquote {
      padding-right: 1em;
      page-break-inside: avoid;
   }

   tr, img {
      page-break-inside: avoid;
   }

   img {
      max-width: 100% !important;
   }

   @page :left {
      margin: 15mm 20mm 15mm 10mm;
   }

   @page :right {
      margin: 15mm 10mm 15mm 20mm;
   }

   p, h2, h3 {
      orphans: 3; widows: 3;
   }

   h2, h3 {
      page-break-after: avoid;
   }
}
</style>



</head>

<body>
<p>Load packages</p>

<pre><code class="r, echo = TRUE">library(knitr)
library(lubridate)
library(dplyr)
library(ggplot2)
</code></pre>

<p>Loading and Reprocessing the data.
Download data if doesn&#39;t exist in current directory</p>

<pre><code class="r, echo=TRUE">if(!file.exists(&quot;activity&quot;)) {
  temp &lt;- tempfile()
  download.file(&quot;https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip&quot;, temp)
  file &lt;- unzip(temp)
  unlink(temp)
}
</code></pre>

<ol>
<li>Read and clean the data for analysis</li>
</ol>

<pre><code class="r, echo=TRUE">activity &lt;- read.csv(&quot;activity.csv&quot;, header = TRUE)
</code></pre>

<ol>
<li>Process the data for analysis</li>
</ol>

<pre><code class="r, echo=TRUE">activity$date &lt;- ymd(activity$date)
data &lt;- tbl_df(activity)
</code></pre>

<p>Mean and Median number of steps taken per day</p>

<ol>
<li>Total number of steps taken per day</li>
</ol>

<pre><code class="r, echo=TRUE">Steps &lt;- data %&gt;%  filter(!is.na(steps)) %&gt;%  group_by(date) %&gt;%  summarize(steps = sum(steps)) %&gt;%print
</code></pre>

<ol>
<li>Histogram of total number of steps taken each day</li>
</ol>

<pre><code class="r, echo=TRUE">hist(Steps$steps, col= &quot;red&quot;, xlab=&quot;Steps Per Day&quot;, main= &quot;Total Steps Per Day&quot;)
</code></pre>

<ol>
<li>Mean of total number of steps taken per day
<code>{r, echo=TRUE}
MeanSteps &lt;- mean(Steps$steps)
print(MeanSteps)
</code>
Answer = 10766.19</li>
</ol>

<p>Median of total number of steps taken per day</p>

<pre><code class="r, echo=TRUE">MedianSteps &lt;- median(Steps$steps)
print(MedianSteps)
</code></pre>

<p>Answer = 10765</p>

<p>Average daily activity pattern</p>

<p>Time series plot of the average number of steps taken</p>

<pre><code class="r, echo=TRUE">steps_interval &lt;- data%&gt;% filter(!is.na(data$steps)) %&gt;% 
  group_by(interval) %&gt;% summarize(steps = mean(steps))
plot(steps_interval$interval, steps_interval$steps, col=&quot;red&quot;,
     xlab=&quot;Interval (5 Min)&quot;, ylab=&quot;Average Number of Steps&quot;, 
     main=&quot;Time Series Plot&quot;, type=&quot;l&quot;)
</code></pre>

<p>The 5-minute interval that, on average, contains the maximum number of steps
First we filter by maximum value of steps and then record corresponding interval</p>

<pre><code class="r, echo=TRUE">intervals &lt;- data%&gt;% filter(!is.na(data$steps)) %&gt;% 
  group_by(interval) %&gt;% summarize(steps = sum(steps)) 

(filter(intervals, steps==max(intervals$steps)))$interval
</code></pre>

<p>Answer = 835th interval</p>

<p>Imputing missing data
1. Total number of missing values in data</p>

<pre><code class="r, echo=TRUE">MissingValues &lt;- length(which(is.na(data$steps)))
</code></pre>

<p>Number of missing values =2304
2. We will use average number of steps in the same 5 Min interval to fill out NA values</p>

<pre><code class="r, echo=TRUE">data_full &lt;- data
nas &lt;- is.na(data_full$steps)
avg_interval &lt;- tapply(data_full$steps, data_full$interval, mean, na.rm=TRUE, simplify=TRUE)
</code></pre>

<ol>
<li>Creating a new data setequal to original data set but with filled missing values</li>
</ol>

<pre><code class="r, echo=TRUE">data_full$steps[nas] &lt;- avg_interval[as.character(data_full$interval[nas])]
sum(is.na(data_full$steps))
</code></pre>

<ol>
<li>Histogram for total number of steps taken each day after imputing missing data</li>
</ol>

<pre><code class="r, echo=TRUE">steps_full &lt;- data_full %&gt;%
  filter(!is.na(steps)) %&gt;%
  group_by(date) %&gt;%
  summarize(steps = sum(steps)) %&gt;%
  print
ggplot(steps_full, aes(x = steps)) +  geom_histogram(fill = &quot;firebrick&quot;, binwidth = 1000) +labs(title = &quot;Histogram of Steps per day after imputing missing values&quot;, x = &quot;Steps per day&quot;, y = &quot;Frequency&quot;)
</code></pre>

<p>Mean and median for the new data set</p>

<pre><code class="r, echo=TRUE">mean_steps_full &lt;- mean(steps_full$steps, na.rm = TRUE)
print(mean_steps_full)
median_steps_full &lt;- median(steps_full$steps, na.rm = TRUE)
print(median_steps_full)
</code></pre>

<p>Imputing missing data didn&#39;t affect the mean and median of our original data set.</p>

<p>Activity pattern on weekdays and weekends
1. Creating a new factor variable weektype to distinguish between weekdays and weekend data.</p>

<pre><code class="r, echo=TRUE">data_full$date &lt;- as.Date(as.character(data_full$date))
data_full &lt;- mutate(data_full, weektype = ifelse(weekdays(data_full$date) == &quot;Saturday&quot; | weekdays(data_full$date) == &quot;Sunday&quot;, &quot;weekend&quot;, &quot;weekday&quot;))
data_full$weektype &lt;- as.factor(data_full$weektype)
head(data_full)
</code></pre>

<p>Creating the panel plot for weekdays and weekends</p>

<pre><code class="r, echo=TRUE">interval_full &lt;- data_full %&gt;%  group_by(interval, weektype) %&gt;%  summarise(steps = mean(steps))
p &lt;- ggplot(interval_full, aes(x=interval, y=steps, color = weektype)) +
  geom_line() +
  facet_wrap(~weektype, ncol = 1, nrow=2)
print(p)
</code></pre>

<p>Subject seems to be more active at the start of week days compared to weekends but is more active throughtout the weekedn than weekdays.</p>

</body>

</html>
