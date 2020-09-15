(* ::Package:: *)

BeginPackage["LatexPlot`"];


Needs["MaTeX`"]
Get["CustomTicks.m"]


(* ::Section:: *)
(*myThemes*)


Themes`AddThemeRules["myPalette", 
DefaultPlotStyle->Thread@Directive[{RGBColor[0.15, 0.5, 1],Hue[0.77, 0.6900000000000001, 0.85],Hue[0.93, 0.72, 1],RGBColor[1., 0.39, 0.08],RGBColor[0, 0.6, 0],RGBColor[0, 0.77, 1],Hue[0.7, 0.5],Hue[0, 1, 1],Hue[1.12],Hue[0.46, 1, 0.86],Hue[Rational[5, 6], 1, Rational[1, 2]],Hue[0.47000000000000003`, 0.77, 0.63]},Thickness[0.001]]];
Themes`AddThemeRules["myArrows", 
DefaultPlotStyle->Thread@Directive[{RGBColor[0.12, 0.42, 1.],RGBColor[0, 0.6, 0],RGBColor[1., 0.39, 0.08],Hue[0.77, 1., 0.98],Hue[0.46, 1, 0.86],Hue[0.9],Hue[1.12],RGBColor[0.11, 0.59, 1],Hue[Rational[5, 6], 1, Rational[1, 2]],Hue[0.47000000000000003`, 0.77, 0.63],Hue[0, 1, 1] ,Hue[0.7, 0.5]},Thickness[0.001]],
AxesStyle->Directive[ Black,Arrowheads[{{Automatic,Automatic, Graphics[Line[{{{-2/3,1/4},{0,0},{-2/3,-1/4}}}]]     }}]],
LabelStyle->{FontFamily->"Latin Modern Math",FontSize->14,  Black},
ImageSize-> 500
];


(* ::Section:: *)
(*Usage*)


LatexPlot::usage="LatexPlot[plot] gives plot in LaTeX style.";


ReStylePlot::usage="ReStylePlot[plot] gets the data of your plot and allows you to set the options again. The function Show[] does not allow to change themes and styles, for example.";


AddLegends::usage="AddLegends[plot, legends] add legends to plot.";


(* ::Section:: *)
(*Definitions*)


Begin["Private`"];


ReStylePlot[p_,line_:Line,op:OptionsPattern[{ListLinePlot,ReStylePlot}]]:=
If[line===Line,ListLinePlot[Cases[Normal@p,Line[x__]:>x,\[Infinity]],op,Options[p]],
ListPlot[Cases[Normal@p,Point[x__]:>x,\[Infinity]],op,Options[p]]]


Options[LatexPlot]={
FontSize->20, 
Thickness->Large,
"closedFrame"->True,
"minorTicks"->False, 
"majorTicks"->True,
Rotate->False,
ImageSize->500,
"leftTicks"->False,
"bottomTicks"->False,
GridLines-> Automatic,
GridLinesStyle->Directive[{Gray,Dashed,Thickness[Small]}],
AxesLabel-> False,
PlotStyle->None,
PlotTheme-> "myPalette",
PlotRange -> Automatic,
"listPlot"-> False,
"logPlot"-> False,
"ticks"->True
};


LatexPlot[plot_,OptionsPattern[]]:=Module[{legends,lineorList,pl,xmin,xmax,ymin,ymax,al,xlabel,ylabel,bt,lt,yticks,xticks},
If[OptionValue["listPlot"]===False,lineorList=Line,lineorList=List];

If[OptionValue[PlotStyle]=!=None,
pl= ReStylePlot[plot,lineorList,PlotStyle-> OptionValue[PlotStyle]],
	If[OptionValue[PlotTheme]=!=None,
	pl= ReStylePlot[plot, lineorList,PlotTheme-> OptionValue[PlotTheme]]]];
	
If[OptionValue[PlotRange]===Full||OptionValue[PlotRange]===All||OptionValue[PlotRange]===Automatic,
{xmin,xmax,ymin,ymax}=Flatten[PlotRange/.First[AbsoluteOptions[plot,PlotRange]]],
If[Length[Flatten[OptionValue[PlotRange]]]==2, {xmin,xmax}=First[PlotRange/.First[AbsoluteOptions[plot,PlotRange]]];{ymin,ymax}=OptionValue[PlotRange],
{xmin,xmax,ymin,ymax}= Flatten[OptionValue[PlotRange]]]];
If[OptionValue[AxesLabel]===False,
xlabel=AbsoluteOptions[plot,AxesLabel][[1,2,1]];ylabel=AbsoluteOptions[plot,AxesLabel][[1,2,2]],
xlabel=OptionValue[AxesLabel][[1]];ylabel=OptionValue[AxesLabel][[2]]
];
If[OptionValue["ticks"]===True,
	bt=OptionValue["bottomTicks"];
	lt=OptionValue["leftTicks"];
	If[OptionValue["logPlot"]===False, 
		auxxticks=LinTicks[xmin,xmax,ShowMinorTicks-> OptionValue["minorTicks"]];
		auxyticks=LinTicks[ymin,ymax,ShowMinorTicks-> OptionValue["minorTicks"]];
		If[OptionValue["majorTicks"]===False,
			topticks={}; rightticks={},
			rightticks=LinTicks[ymin,ymax,ShowMinorTicks-> OptionValue["minorTicks"],ShowMinorTickLabels->False,ShowTickLabels->False];
			topticks=LinTicks[xmin,xmax,ShowMinorTicks->  OptionValue["minorTicks"],ShowMinorTickLabels->False,ShowTickLabels->False]
		],
		auxxticks=LinTicks[xmin,xmax,ShowMinorTicks-> OptionValue["minorTicks"]];
		auxyticks=LogTicks[ymin,ymax,ShowMinorTicks-> OptionValue["minorTicks"]];
		If[OptionValue["majorTicks"]===False,
			topticks={}; rightticks={},
			rightticks=If[OptionValue["leftTicks"]===False, LogTicks[ymin,ymax,ShowMinorTicks-> OptionValue["minorTicks"],ShowMinorTickLabels->False,ShowTickLabels->False],
						Table[{OptionValue["leftTicks"][[i]],"" },{i,1,Length[OptionValue["leftTicks"]]}]];
			topticks=LinTicks[xmin,xmax,ShowMinorTicks->  OptionValue["minorTicks"],ShowMinorTickLabels->False,ShowTickLabels->False]
		]
	];

	xticks=If[bt===False, auxxticks ,Table[{bt[[i]],bt[[i]]},{i,1,Length[bt]}]];
	yticks=If[lt===False, auxyticks,Table[{lt[[i]],lt[[i]]},{i,1,Length[lt]}]];
	ruleFrameTicks = FrameTicks->{
				{MapAt[Function[x,MaTeX[x,Magnification->OptionValue[FontSize]/12]],yticks,{{All,2}}],rightticks},
				{MapAt[Function[x,MaTeX[x,Magnification->OptionValue[FontSize]/12]],xticks,{{All,2}}],topticks}
				},
	ruleFrameTicks = FrameTicks -> None
];
(*Why would someone use both Thickness and AbsoluteTickness?*)
Show[
pl/.{Thickness[x_]->Thickness[OptionValue[Thickness]],Axes->False,AxesLabel->None,PlotLabel->None},
Frame->{{True,OptionValue["closedFrame"]},{True,OptionValue["closedFrame"]}},
FrameLabel->{
If[xlabel===None,xlabel, MaTeX[StringForm["``",xlabel],Magnification->1.2*OptionValue[FontSize]/12]],
If[ylabel===None,ylabel, MaTeX[StringForm["``",ylabel],Magnification->1.2*OptionValue[FontSize]/12]]
},
FrameStyle->Directive[Black,Thickness[Medium]],
RotateLabel->OptionValue[Rotate],
ruleFrameTicks,				
PlotRange->{{xmin,xmax},{ymin,ymax}},
GridLines->OptionValue[GridLines],
GridLinesStyle->OptionValue[GridLinesStyle],
ImageSize-> OptionValue[ImageSize]
]
]


RasterizeLatexPlot[plot_,op:OptionsPattern[LatexPlot]]:= Rasterize[LatexPlot[plot, op], ImageResolution->500]  (*Why doesn't this work? I'm tired of writting Rasterize!*)


Options[AddLegends]={
"columns"->Automatic,
"colors"->"myPalette",
FontSize->20,
"text"->False
};


AddLegends[plot_,leg_, OptionsPattern[]]:=(Module[{legends,colors},
If[OptionValue["text"]===True,legends=MapAt[Function[x,MaTeX[StringForm["\\text{``}",x],Magnification->OptionValue[FontSize]/12]], leg, All], legends=MaTeX[leg,Magnification->OptionValue[FontSize]/12] ];

If[OptionValue["colors"]===Automatic,colors=Table[plot[[1,2,1,i,1]],{i,3,Length[plot[[1,2,1]]]}],colors=OptionValue["colors"]];
If[OptionValue["colors"]==="myPalette", colors={RGBColor[0.15, 0.5, 1],Hue[0.77, 0.6900000000000001, 0.85],Hue[0.93, 0.72, 1],RGBColor[1., 0.39, 0.08],RGBColor[0, 0.6, 0],RGBColor[0, 0.77, 1],Hue[0.7, 0.5],Hue[0, 1, 1],Hue[1.12],Hue[0.46, 1, 0.86],Hue[Rational[5, 6], 1, Rational[1, 2]],Hue[0.47000000000000003`, 0.77, 0.63]}];
Legended[plot,LineLegend[colors,legends , LegendLayout->{"Column",OptionValue["columns"]}]]
])




End[]
EndPackage[]
