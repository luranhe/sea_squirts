#!/usr/local/bin/MathematicaScript -script

ParallelDo[
  f=ToExpression@Import[FileNameJoin[{DirectoryName[file],"wavefronts.txt"}]];
  b=ToExpression@Import[FileNameJoin[{DirectoryName[file],"wavebacks.txt"}]];
  g=Graphics[{Red,Point[f],Blue,Point[b]},Axes->True,AxesOrigin->{0,0},
    AspectRatio->Last@Last@BoundingRegion[Join[f,b],"MinRectangle"]/400,
    Ticks->{Automatic,Range[0,#2,1000]&},ImageSize->500];
  Export[file,g],{file,$ScriptCommandLine[[2;;-1]]}]
