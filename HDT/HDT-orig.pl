#--- PART 1: Read data, compute features, populate hash tables
open(IN, "<", "repeat.tsv") or die;
$ID=0;
$sum_pv=0;
<IN>;

while($i=<IN>){
  $i=~s/\n//;
  $i=lc($i);
  @aux=split(/\t/,$i);
  $pv=$aux[3];
  $pv=log(1+$pv);
  $url=$aux[1];
  if ($url =~ "/blogs/") {
    $type="BLOG";
  } else {
    $type="OTHER";
  }
  #--- clean article titles, remove stop words
  $title=$aux[0];
  $title=~s/"/ /g;
  $title=~s/\?/ \? /g;
  $title=~s/\:/ /g;
  $title=~s/\./ /g;
  $title=~s/\(/ /g;
  $title=~s/\)/ /g;
  $title=~s/\,/ /g;
  $title = " $title ";
  $title=~s/ a / /g;
  $title=~s/ the / /g;
  $title=~s/ for / /g;
  $title=~s/ in / /g;
  $title=~s/ and / /g;
  $title=~s/ or / /g;
  $title=~s/ is / /g;
  $title=~s/ in / /g;
  $title=~s/ are / /g;
  $title=~s/ of / /g;
  $title=~s/ / /g;
  #---break down article title into keyword tokens
  @aux2=split(" ",$title);
  $nkw=($#aux2) +1;
  #--- One token at keywords: feature computation
  for ($k=0; $k<$nkw; $k++) {
    $word = $aux2[$k]."\t"."N/A"."\t"."$type";
    $list{$word}++;
    $listpv{$word}+=$pv;

    $max=$listpvmax{$word};
    if ($max eq "") { $max=-1; }
    $min=$listpvmin{$word};
    if ($min eq "") { $min=99999999; }
    if ($pv > $max) {
      $max=$pv; 
      $listpvmax{$word}=$max;
    }
    if ($pv < $min) {
      $min=$pv; 
      $listpvmin{$word}=$min;
    }
    $listid{$word}=$listid{$word}."~$ID";

  }
  #--- two tokens keywords: feature computation
  for ($k=0; $k<$nkw-1; $k++) {
    $word1 = $aux2[$k];
    $word2 = $aux2[$k+1];
    $word = $word1."\t".$word2."\t"."$type";
    $list{$word}++;
    $listpv{$word}+=$pv;

    $max=$listpvmax{$word};
    if ($max eq "") { $max=-1; }
    $min=$listpvmin{$word};
    if ($min eq "") { $min=99999999; }
    if ($pv > $max) {
      $max=$pv; 
      $listpvmax{$word}=$max;
    }
    if ($pv < $min) {
      $min=$pv; 
      $listpvmin{$word}=$min;
    }
    $listid{$word}=$listid{$word}."~$ID";

  }
  #--- compute summary stats, update article tables
  $articleTitle[$ID]=$title;
  $articlepv[$ID]=$pv;
  $sum_pv+=$pv;
  $ID++;
}
close(IN);
$nArticles=$ID;
$avg_pv = $sum_pv / $nArticles;

#--- PART 2: Identify feature values & articles marked as GOOD
for ($ID=0; $ID<$nArticles; $ID++) { 
  $articleFlag[$ID]="BAD"; 
}
open(OUT,">", "hdt-out2.txt");
open(OUT2,">", ">hdt-reasons.txt");
$nidx=0;
$nidx_Good=0;
foreach $idx (keys(%list)) { # idx is a feature value obtained in PART 1
  $n = $list{$idx}; # number of articles with this feature
  $avg = $listpv{$idx} / $n;
  $min = $listpvmin{$idx};
  $max = $listpvmax{$idx};
  $idlist = $listid{$idx};
  $nidx++; # number of feature values
  # try 0 or 5 instead of 3 in IF statement below, in "$n > 3"
  # see impact on $errorRate and $aggregationFactor
  if ( (($n > 3)&&($n < 8)&&($min > 6.9)&&($avg > 7.6)) || 
       (($n >= 8)&&($n < 16)&&($min > 6.7)&&($avg > 7.4)) ||
       (($n >= 16)&&($n < 200)&&($min > 6.1)&&($avg > 7.2)) ) {
    print OUT "$idx\t$list{$idx}\t$avg\t$min\t$max\t$idlist\n";
    $nidx_Good++; # feature value marked as good

    @aux=split("~",$idlist);
    $nIDS=($#aux) +1;
    for ($k=1; $k<$nIDS; $k++) {
      $ID=$aux[$k];
      $title=$articleTitle[$ID];
      $pv=$articlepv[$ID];
      $articleTitle[$ID]=$title;
      print OUT2 "$title\t$pv\t$idx\t$n\t$avg\t$min\t$max\n";
      $articleFlag[$ID]="GOOD"; # article marked as good
    }
  }
}
close(OUT2);
close(OUT);

#--- PART 3: Compute final summary stats & confusion matrix
$pv_threshold = 7.1; # corresponds to 1,200 page views per article 
$n1 = 0;
$n2 = 0;
$pv1 = 0;
$pv2 = 0;
$FalsePositive = 0;
$FalseNegative = 0;
$m1 = 0;
$m2 = 0;
for ($ID=0; $ID < $nArticles; $ID++) {
  $pv=$articlepv[$ID]; 
  if ($articleFlag[$ID] eq "GOOD") {
    $n1++;
    $pv1+=$pv;
    if ($pv < $pv_threshold) { 
      $FalsePositive++;
    }
  } else {
    $n2++;
    $pv2+=$pv;
    if ($pv > $pv_threshold) { 
      $FalseNegative++;
    }
  }
  if ($pv > $pv_threshold) { 
    $m1++;
  } else {
    $m2++;
  }
}
#--- print performance results
$avg_pv1 = $pv1/$n1;
$avg_pv2 = $pv2/$n2;
$errorRate = ($FalsePositive + $FalseNegative)/$nArticles;
$aggregationFactor = ($nidx/$nidx_Good)/($nArticles/$n1);
print "Average pv: $avg_pv\n";
print "Number of articles marked as good: $n1 (real number is $m1)\n"; 
print "Number of articles marked as bad: $n2 (real number is $m2)\n";
print "Avg pv: articles marked as good: $avg_pv1\n";
print "Avg pv: articles marked as bad: $avg_pv2\n";
print "Number of false positive: $FalsePositive (bad marked as good)\n";
print "Number of false negative: $FalseNegative (good marked as bad)\n"; 
print "Number of articles: $nArticles\n";
print "Error Rate: $errorRate\n";
print "Number of feature values: $nidx (marked as good: $nidx_Good)\n";
print "Aggregation factor: $aggregationFactor\n";
