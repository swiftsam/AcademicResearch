GET DATA 
  /TYPE=XLS 
  /FILE='T:\files\docs\Research\PECB\PECB Data 2009-02-13.xls' 
  /SHEET=name 'Wide' 
  /CELLRANGE=full 
  /READNAMES=on 
  /ASSUMEDSTRWIDTH=32767. 
DATASET NAME wide.

GET DATA 
  /TYPE=XLS 
  /FILE='T:\files\docs\Research\PECB\PECB Data 2009-02-13.xls' 
  /SHEET=name 'Long' 
  /CELLRANGE=full 
  /READNAMES=on 
  /ASSUMEDSTRWIDTH=32767.
DATASET NAME long.

VALUE LABELS PERFCAT 1 'Below Average' 2 'Average' 3 'Above Average.
VALUE LABELS SITCAT 1 'Low Average GPA' 2 'Medium Average GPA' 3 'High Average GPA'.
VARIABLE LABELS PERFCAT 'Performance Category'.
VARIABLE LABELS SITCAT 'Situation Category'.


DATASET ACTIVATE wide.

GLM success_c2 success_c1 success_c6 success_c5 success_c9 success_c3 success_c4 success_c7 success_c8 
  /WSFACTOR=SIT 3 Polynomial PERF 3 Polynomial
  /PRINT=ETASQ
  /METHOD=SSTYPE(3) 
  /CRITERIA=ALPHA(.05) 
  /WSDESIGN=SIT PERF SIT*PERF.

GLM probAccept_c2 probAccept_c1 probAccept_c6 probAccept_c5 probAccept_c9 probAccept_c3 probAccept_c4 probAccept_c7 probAccept_c8 
  /WSFACTOR=SIT 3 Polynomial PERF 3 Polynomial 
  /METHOD=SSTYPE(3) 
  /PRINT=ETASQ 
  /CRITERIA=ALPHA(.05) 
  /WSDESIGN=SIT PERF SIT*PERF.

GLM Accept_c2 Accept_c1 Accept_c6 Accept_c5 Accept_c9 Accept_c3 Accept_c4 Accept_c7 Accept_c8 
  /WSFACTOR=SIT 3 Polynomial PERF 3 Polynomial 
  /METHOD=SSTYPE(3)
  /PRINT=ETASQ 
  /CRITERIA=ALPHA(.05) 
  /WSDESIGN=SIT PERF SIT*PERF.

DATASET ACTIVATE long. 
DESCRIPTIVES VARIABLES=success probAccept Accept 
  /SAVE 
  /STATISTICS=MEAN STDDEV MIN MAX.

* sucess : mean = 3.70, stdev = 1.417 *
* probAccept : mean = 43.38, stdev = 26.098 *
* Accept : mean = .44, stdev = .498 *

RELIABILITY
  /VARIABLES=Zsuccess, ZprobAccept, ZAccept.

COMPUTE ZEvaluation = MEAN(Zsuccess, ZprobAccept, ZAccept).
EXECUTE.

DATASET ACTIVATE wide.

COMPUTE Z1=MEAN((success_c1-3.7)/1.417,(probAccept_c1-43.38)/26.098,(Accept_c1-.44)/.498).
COMPUTE Z2=MEAN((success_c2-3.7)/1.417,(probAccept_c2-43.38)/26.098,(Accept_c2-.44)/.498).
COMPUTE Z3=MEAN((success_c3-3.7)/1.417,(probAccept_c3-43.38)/26.098,(Accept_c3-.44)/.498).
COMPUTE Z4=MEAN((success_c4-3.7)/1.417,(probAccept_c4-43.38)/26.098,(Accept_c4-.44)/.498).
COMPUTE Z5=MEAN((success_c5-3.7)/1.417,(probAccept_c5-43.38)/26.098,(Accept_c5-.44)/.498).
COMPUTE Z6=MEAN((success_c6-3.7)/1.417,(probAccept_c6-43.38)/26.098,(Accept_c6-.44)/.498).
COMPUTE Z7=MEAN((success_c7-3.7)/1.417,(probAccept_c7-43.38)/26.098,(Accept_c7-.44)/.498).
COMPUTE Z8=MEAN((success_c8-3.7)/1.417,(probAccept_c8-43.38)/26.098,(Accept_c8-.44)/.498).
COMPUTE Z9=MEAN((success_c9-3.7)/1.417,(probAccept_c9-43.38)/26.098,(Accept_c9-.44)/.498).
EXECUTE.

GLM Z2 Z1 Z6 Z5 Z9 Z3 Z4 Z7 Z8 
  /WSFACTOR=SIT 3 Polynomial PERF 3 Polynomial 
  /METHOD=SSTYPE(3)
  /PRINT=ETASQ 
  /CRITERIA=ALPHA(.05) 
  /WSDESIGN=SIT PERF SIT*PERF.

DATASET ACTIVATE long. 
* Chart Builder. 
GGRAPH 
  /GRAPHDATASET NAME="graphdataset" VARIABLES=PerfCat MEAN(ZEvaluation)[name="MEAN_ZEvaluation"] SitCat MISSING=LISTWISE REPORTMISSING=NO 
  /GRAPHSPEC SOURCE=INLINE. 
BEGIN GPL 
  SOURCE: s=userSource(id("graphdataset")) 
  DATA: PerfCat=col(source(s), name("PerfCat"), unit.category()) 
  DATA: MEAN_ZEvaluation=col(source(s), name("MEAN_ZEvaluation")) 
  DATA: SitCat=col(source(s), name("SitCat"), unit.category()) 
  COORD: rect(dim(1,2), cluster(3,0)) 
  GUIDE: axis(dim(3), label("Performance Category")) 
  GUIDE: axis(dim(2), label("Mean Z-score evaluation")) 
  GUIDE: legend(aesthetic(aesthetic.color.interior), label("Situation Category")) 
  SCALE: linear(dim(2), include(0)) 
  ELEMENT: interval(position(SitCat*MEAN_ZEvaluation*PerfCat), color.interior(SitCat), shape.interior(shape.square)) 
END GPL.

CROSSTABS 
  /TABLES=SitCat BY PerfCat BY Accept 
  /FORMAT=AVALUE TABLES 
  /CELLS=COUNT 
  /COUNT ROUND CELL.

DATASET ACTIVATE wide. 
DESCRIPTIVES VARIABLES=gpaavailable gpause 
  /STATISTICS=MEAN STDDEV MIN MAX

GRAPH 
  /HISTOGRAM(NORMAL)=gpause.

GRAPH 
  /HISTOGRAM(NORMAL)=gpaavailable.
