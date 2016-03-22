<!DOCTYPE html>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>

<title>Data Processing</title>

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
<p>##Synopsis 
This study analyses what are the meteorological events more harmful to properties, crops and human health. The data has been taken from National Oceanic and Atmospheric Administration (NOAA  <a href="http://www.noaa.gov/">http://www.noaa.gov/</a>) which has been gathering information about enviromental events in EEUU since 1950. The study disregards records before 1996 as data before that year is mainly focus on tornados. According to the data it might be concluded that, in general, most harmful enviromental events for properties, crops and human health are floods, hurricanes, typhoons and tornados.</p>

<h2>Data Processing</h2>

<p>The Storm Event Database from NOAA covers enviromental events from January 1950 to September 2015. Hovewer, most of the events recorded before 1996 were only tornados. From 1996 onwards, all type of events have been taken into account. <a href="http://www.ncdc.noaa.gov/stormevents/details.jsp?type=eventtype">http://www.ncdc.noaa.gov/stormevents/details.jsp?type=eventtype</a>. So, in order to deal with a database more representative of every enviromental event the information before 1996 has been discarded.</p>

<pre><code class="r, collapse=TRUE, tidy=TRUE">&#39;download.file(&quot;https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2&quot;,&quot;StormData.csv&quot;)&#39;
data&lt;-read.csv(&quot;./StormData.csv&quot;)

library(lubridate)
&#39;transform BGN_DATE column into a convenient date format to remove those events before 1996&#39;
data$BGN_DATE&lt;-as.Date(as.character(data$BGN_DATE),&quot;%m/%d/%Y %H:%M:%S&quot;)
dataf&lt;-data[year(data$BGN_DATE)&gt;=1996,]
</code></pre>

<p>Now it is required to calculate the economic cost of each event. The database presents a column for the cost of damages on properties and another for crops. However, these columns are not complete, they miss the multiplier (whether the values reported are hundreds, thousands, millions&hellip;) so another 2 columns present these multipliers. In order to transform these multipliers into a numeric values, the following process has been performed:</p>

<pre><code class="r, cache=TRUE, collapse=TRUE, tidy=TRUE">&#39;Data from 1996 onwards, does not use the numerical multipliers (0,1,2..) from PROPDMGEXP and PROPDMGEXP as it is shown:&#39;
unique(dataf$PROPDMGEXP)
unique(dataf$CROPDMGEXP)
&#39;Therefore, in the following function, numerical multipliers have been disregarded.&#39;

f &lt;- function (x) {
  &#39;toupper function converts whatever letter to capital letter&#39;
  if (toupper(x[2])==&quot;H&quot;)
  {y&lt;-as.numeric(x[1])*100}
  else if (toupper(x[2])==&quot;K&quot;)
  {y&lt;-as.numeric(x[1])*1000}
  else if (toupper(x[2])==&quot;M&quot;)
  {y&lt;-as.numeric(x[1])*1000000}
  else if (toupper(x[2])==&quot;B&quot;)
  {y&lt;-as.numeric(x[1])*1000000000}
  else 
  {y&lt;-as.numeric(x[1])}
  y
}

&#39;now, Columns PROPDMG and CROPDMG are changed to accound for the information of the columns PROPDMGEXP and CROPDMGEXP&#39;

dataf$PROPDMG&lt;-apply(data.frame(dataf$PROPDMG,dataf$PROPDMGEXP),1,f)
dataf$CROPDMG&lt;-apply(data.frame(dataf$CROPDMG,dataf$CROPDMGEXP),1,f)

&#39;A column with the total economic damage it is worked out.&#39;
EconomicDMG&lt;-dataf$PROPDMG+dataf$CROPDMG
</code></pre>

<p>Once it has been calculated the total economic cost for each reported event since 1996 the information is summarized by events, so a comparation may be done.</p>

<pre><code class="r, cache=TRUE, collapse=TRUE, warning=FALSE, tidy=TRUE">finalSet&lt;-data.frame(EVTYPE=dataf$EVTYPE, FATALITIES=dataf$FATALITIES,INJURIES=dataf$INJURIES,PROPDMG=dataf$PROPDMG,CROPDMG=dataf$CROPDMG, EconomicDMG)
finalSet$EVTYPE&lt;-factor(finalSet$EVTYPE,levels=finalSet$EVTYPE[order(finalSet$EconomicDMG,decreasing=TRUE)])

a&lt;-aggregate(EconomicDMG ~ EVTYPE,finalSet,mean)
b&lt;-aggregate(cbind(FATALITIES,INJURIES,EconomicDMG) ~ EVTYPE,finalSet,sum)
b$EVTYPE&lt;-factor(b$EVTYPE,levels=b$EVTYPE[order(b$EconomicDMG,decreasing=TRUE)])
cc&lt;-data.frame(b,AvgDMGperEvent=a$EconomicDMG)
</code></pre>

<h2>Results</h2>

<p>The damages of an enviromental event are assessed according to two different criteria:</p>

<ol>
<li>Human health damages which are assessed by quantifying the number of injures and fatalities.</li>
<li>Economic damages which are assessed by calculating the cost of the damages to properties and crops.</li>
</ol>

<h3>1 Human health damage:</h3>

<p>In order to assess the damage impact to human health the driver parameter for the comparison is injuries even though fatalities is more severe. As this study assumes the hypothesis that injured people have higher impact for government in terms of cost, time and means than fatalities.
The table below is arranged according to the events that gather more records of injured people. It is depicted that the top 3 most hazardous enviromental events for human health are tornados, floods and excessive heat. </p>

<pre><code class="r, collapse=TRUE, tidy=TRUE">PeopleDMG&lt;-b[order(b$INJURIES,decreasing=TRUE),]
head(PeopleDMG)
</code></pre>

<h3>2 Economic damage</h3>

<p>In the left graph below, The most hazardous meteorological events are ordered according to the total economic damage. 
For instance, the case of the flood. Even though its averaged cost per event is low, it has been reported more often than others, thus being the most hazardous event for properties and crops. It is followed by storm surge and hurricane/typhoon.</p>

<p>It is worth noting that next to the graph of the cost of damages is the total number of injures and fatalities reported for each of those enviromental events.</p>

<pre><code class="r, collapse=TRUE, fig.align=&#39;center&#39;, tidy=TRUE">
library (ggplot2)
&#39;Most harmful enviromental events to properties and crops&#39;
p1&lt;- ggplot(data=cc[1:10,],aes(x=EVTYPE,col=&#39;Type of cost&#39;))+ggtitle(&quot;Effects on properties&quot;)+geom_point(aes(y=EconomicDMG,col=&quot;Total&quot;))+geom_point(aes(y=AvgDMGperEvent, col=&quot;Averaged&quot;))+theme(axis.title.y=element_blank(),plot.title = element_text(lineheight=.8, face=&quot;bold&quot;))+scale_y_log10()+ylab(&quot;Cost of damage in $ (Log10)&quot;)+coord_flip()

&#39;Effect on public health&#39;
library(reshape2)
mostDMG&lt;-cc$EVTYPE[1:10]
PublicHealth&lt;-melt(b,id=c(&quot;EVTYPE&quot;,&quot;EconomicDMG&quot;))
PublicHealth&lt;-PublicHealth[PublicHealth$EVTYPE%in%mostDMG,]
PublicHealth$EVTYPE&lt;-factor(PublicHealth$EVTYPE)

p2&lt;- ggplot(PublicHealth,aes(x=EVTYPE,y=value, fill=variable))+ggtitle(&quot;Effect on public health&quot;)+geom_bar(stat=&quot;identity&quot;,position=&quot;dodge&quot;)+scale_y_log10()+ylab(&quot;People (Log10)&quot;)+theme(axis.title.y=element_blank(),plot.title = element_text(lineheight=.8, face=&quot;bold&quot;))+ scale_fill_discrete(&quot;&quot;)+theme(axis.text.y=element_blank(),axis.title.y=element_blank())+coord_flip()

library(gridExtra)
grid.arrange(p1,p2,ncol=2,widths=c(2,1))
</code></pre>

<h3>Conclusion</h3>

<p>Each one of the previous categories should have different policy actions, in the case of an event more hazardous for human health policies of risk awarness among population should be followed.
In summary, it can be concluded tha the more dangerous events for people are floods, huricanes, typhoons and tornados.</p>

</body>

</html>
