#!/usr/bin/env wolframscript
(* ::Package:: *)

ParallelDo[
	d=DirectoryName[file];
	f=ToExpression@Import[FileNameJoin[{d,"wavefronts.txt"}]];
	b=ToExpression@Import[FileNameJoin[{d,"wavebacks.txt"}]];
	g=Graphics[{Red,Point[f],Blue,Point[b]},Axes->True,AxesOrigin->{0,0},
		(* Fix aspect ratio *)
		AspectRatio->Last@Last@BoundingRegion[Join[f,b],"MinRectangle"]/10,
		(* Ticks every 10 seconds, width=500 *)
		Ticks->{Automatic,Range[0,#2,10]&},ImageSize->500];
	(* Repeat over all specified destination files *)
	Export[file,g],{file,$ScriptCommandLine[[2;;]]}]
