(* ::Package:: *)

(*
  CookeTripletDesigner.wl
  Modello preliminare, non destinato alla fabbricazione.
  Unita: millimetri; lunghezze d'onda: micrometri.
  Convenzione: luce da sinistra a destra; R>0 se il centro e a destra.
*)

ClearAll["Global`*"];

(* Righe di Fraunhofer usate nel calcolo. *)
lambdaF = 0.4861327; lambdad = 0.5875618; lambdaC = 0.6562725;
wavelengths = {lambdaF, lambdad, lambdaC};

(* Catalogo minimo SCHOTT incorporato: coefficienti Sellmeier ufficiali.
   n^2 - 1 = Sum[B_i lambda^2/(lambda^2-C_i)], lambda in micrometri.
   UpdateSchottCatalog[] permette di sostituirlo col catalogo AGF corrente. *)
schottCatalog = <|
  "N-SK16" -> <|"nd" -> 1.62041, "Vd" -> 60.32,
    "B" -> {1.34317774, 0.241144399, 0.994317969},
    "C" -> {0.00704687339, 0.0229005000, 92.7508526}|>,
  "SF2" -> <|"nd" -> 1.64769, "Vd" -> 33.85,
    "B" -> {1.403018210, 0.231767504, 0.939056586},
    "C" -> {0.010579547, 0.0493226978, 112.40595500}|>,
  "F2" -> <|"nd" -> 1.62004, "Vd" -> 36.37,
    "B" -> {1.34533359, 0.209073176, 0.937357162},
    "C" -> {0.00997743871, 0.0470450767, 111.886764}|>
|>;

schottZemaxURL = "https://media.schott.com/api/public/content/a79c07aa61da4c05a2c0bbab93d09a7f?download=true&v=3b65e351";
programDirectory = If[StringQ[$InputFileName] && StringLength[$InputFileName] > 0,
  DirectoryName[$InputFileName], Directory[]];
schottCacheFile = FileNameJoin[{programDirectory, "SchottCatalogCache.wl"}];

(* Lettura del formato AGF/ZEMAX distribuito da SCHOTT. *)
ClearAll[ImportSchottAGF, UpdateSchottCatalog];
ImportSchottAGF[file_String] := Module[
  {lines, result = <||>, current = None, tokens, values, line},
  lines = Import[file, "Lines"];
  Do[
    line = StringTrim[raw];
    Which[
      StringStartsQ[line, "NM "],
        tokens = StringSplit[line]; current = ToUpperCase[tokens[[2]]];
        AssociateTo[result, current -> <|
          "nd" -> ToExpression[tokens[[5]], InputForm],
          "Vd" -> ToExpression[tokens[[6]], InputForm]|>],
      StringStartsQ[line, "CD "] && StringQ[current],
        values = ToExpression[#, InputForm] & /@ Rest[StringSplit[line]];
        If[Length[values] >= 6,
          AssociateTo[result, current -> Join[result[current], <|
            "B" -> values[[1 ;; 3]], "C" -> values[[4 ;; 6]]|>]]]
    ], {raw, lines}];
  Select[result, KeyExistsQ[#, "B"] && KeyExistsQ[#, "C"] &]
];

UpdateSchottCatalog[] := Module[{zip, dir, agf, downloaded},
  Print["Scaricamento del catalogo ottico ufficiale SCHOTT..."];
  dir = CreateDirectory[];
  zip = FileNameJoin[{dir, "schott-zemax.zip"}];
  Quiet@Check[URLDownload[schottZemaxURL, zip],
    Print["Scaricamento non riuscito; resta attiva la cache locale."];
    Return[$Failed]];
  Quiet@Check[ExtractArchive[zip, dir],
    Print["Impossibile estrarre il catalogo SCHOTT."]; Return[$Failed]];
  agf = FileNames["*.agf", dir, Infinity, IgnoreCase -> True];
  If[agf === {}, Print["Nessun file AGF trovato nell'archivio."]; Return[$Failed]];
  downloaded = ImportSchottAGF[First[agf]];
  If[Length[downloaded] == 0,
    Print["Il catalogo AGF non contiene record utilizzabili."]; Return[$Failed]];
  schottCatalog = Join[schottCatalog, downloaded];
  Put[downloaded, schottCacheFile];
  Print[Length[downloaded], " vetri SCHOTT caricati e salvati in cache."];
  downloaded
];

If[FileExistsQ[schottCacheFile],
  schottCatalog = Join[schottCatalog,
    Quiet@Check[Get[schottCacheFile], <||>]]];

ClearAll[glassIndex, glassData];
glassAliases = <|"SK16" -> "N-SK16"|>;
glassData[name_String] := Module[{key = ToUpperCase[name]},
  key = Lookup[glassAliases, key, key];
  Lookup[schottCatalog, key, Missing["Glass", name]]];
glassIndex[name_String, lambda_?NumericQ] := Module[{g = glassData[name]},
  If[MissingQ[g], Return[$Failed]];
  Sqrt[1. + Total[g["B"] lambda^2/(lambda^2 - g["C"])]]
];

glassNames = {"N-SK16", "SF2", "N-SK16"};

ClearAll[GlassReport];
GlassReport[name_String] := Module[{g = glassData[name]},
  If[MissingQ[g], Return[g]];
  <|"Name" -> ToUpperCase[name], "nd" -> g["nd"], "Vd" -> g["Vd"],
    "nF" -> glassIndex[name, lambdaF], "ndSellmeier" -> glassIndex[name, lambdad],
    "nC" -> glassIndex[name, lambdaC]|>];

(* Prescrizione normalizzata derivata dal Cooke dimostrativo 50 mm f/5,
   SK16-F2-SK16. Le due curvature della lente negativa sono rimappate da
   F2 a SF2 conservando la potenza di ciascuna superficie alla riga d. *)
referenceF2Radii = {22.01359, -435.76044, -22.21328, 20.29192,
  79.68360, -18.39533};
sf2PowerFactor = (glassData["SF2"]["nd"] - 1)/(glassData["F2"]["nd"] - 1);
referenceSF2Radii = ReplacePart[referenceF2Radii,
  {3 -> sf2PowerFactor referenceF2Radii[[3]],
   4 -> sf2PowerFactor referenceF2Radii[[4]]}];

(* Prescrizione iniziale di circa 50 mm di focale.
   r: sei raggi; t: tre spessori; g12,g23: spazi d'aria;
   stopGap: distanza del diaframma dalla quarta superficie;
   bfl: distanza dell'ultima superficie dal piano immagine. *)
start = <|
  "r" -> referenceSF2Radii,
  "t" -> {3.25896, 0.99997, 2.95208},
  "g12" -> 6.00755, "g23" -> 4.75041, "stopGap" -> 0.0,
  "bfl" -> 42.20778, "semiAperture" -> 5.0,
  "glassNames" -> glassNames, "targetFocalLength" -> 50.0
|>;

ClearAll[vertices, stopZ, surfaceData];
vertices[p_Association] := Module[{t = p["t"]},
  {0., t[[1]], t[[1]] + p["g12"], t[[1]] + p["g12"] + t[[2]],
   t[[1]] + p["g12"] + t[[2]] + p["g23"],
   Total[t] + p["g12"] + p["g23"]}
];
stopZ[p_Association] := vertices[p][[4]] + p["stopGap"];

surfaceData[p_Association, lambda_?NumericQ] := Module[{z, ng},
  z = vertices[p];
  ng = glassIndex[#, lambda] & /@ p["glassNames"];
  MapThread[Association["z" -> #1, "R" -> #2, "n1" -> #3, "n2" -> #4] &,
    {z, p["r"], {1., ng[[1]], 1., ng[[2]], 1., ng[[3]]},
     {ng[[1]], 1., ng[[2]], 1., ng[[3]], 1.}}]
];

(* Calcolo paraxiale con il vettore {altezza, n*angolo}.
   La focale equivalente in aria e -1/C, con C elemento (2,1)
   della matrice ABCD completa. *)
ClearAll[paraxialMatrix, effectiveFocalLength];
paraxialMatrix[p_Association, lambda_?NumericQ : lambdad] := Module[
  {ss, z, matrix = IdentityMatrix[2], refraction, translation, i, distance},
  ss = surfaceData[p, lambda]; z = vertices[p];
  refraction[n1_, n2_, radius_] := {{1., 0.}, {-(n2 - n1)/radius, 1.}};
  translation[distance_, n_] := {{1., distance/n}, {0., 1.}};
  Do[
    matrix = refraction[ss[[i, "n1"]], ss[[i, "n2"]], ss[[i, "R"]]] . matrix;
    If[i < Length[ss],
      distance = z[[i + 1]] - z[[i]];
      matrix = translation[distance, ss[[i, "n2"]]] . matrix],
    {i, Length[ss]}];
  matrix
];
effectiveFocalLength[p_Association, lambda_?NumericQ : lambdad] :=
  -1./paraxialMatrix[p, lambda][[2, 1]];
ClearAll[backFocalLength];
backFocalLength[p_Association, lambda_?NumericQ : lambdad] := Module[
  {m = paraxialMatrix[p, lambda]}, -m[[1, 1]]/m[[2, 1]]];

(* Moltiplica tutte le quote geometriche per lo stesso fattore. I vetri e
   gli angoli di campo non cambiano. Cosi restano invariati forma, f/# e
   aberrazioni relative. *)
ClearAll[scalePrescription, scaleToFocalLength];
scalePrescription[p_Association, factor_?NumericQ] /; factor > 0 := Join[p, <|
  "r" -> factor p["r"], "t" -> factor p["t"],
  "g12" -> factor p["g12"], "g23" -> factor p["g23"],
  "stopGap" -> factor p["stopGap"], "bfl" -> factor p["bfl"],
  "semiAperture" -> factor p["semiAperture"],
  "targetFocalLength" -> factor Lookup[p, "targetFocalLength",
    effectiveFocalLength[p, lambdad]]|>];
scaleToFocalLength[p_Association, target_?NumericQ] /; target > 0 :=
  scalePrescription[p, target/effectiveFocalLength[p, lambdad]];

(* Generatore del punto iniziale. Conserva, alla riga d, la potenza di
   ciascuna superficie del riferimento SK16-F2-SK16; poi impone focale,
   fuoco posteriore e apertura richiesti. *)
ClearAll[GenerateCookeStart];
GenerateCookeStart[target_?NumericQ, fNumber_?NumericQ,
   names : {__String} : {"N-SK16", "SF2", "N-SK16"}, maxField_ : 20.,
   targetBFL_ : Automatic] /;
   target > 0 && fNumber > 0 && NumericQ[maxField] && maxField >= 0 &&
    (targetBFL === Automatic || (NumericQ[targetBFL] && targetBFL > 0)) &&
    Length[names] == 3 := Module[
  {refNames = {"N-SK16", "F2", "N-SK16"}, refN, newN,
   refBefore, refAfter, newBefore, newAfter, powers, radii, p, base,
   candidate, solution, scale, gapScale},
  If[AnyTrue[names, MissingQ[glassData[#]] &],
    Print["Vetro non presente nel catalogo: ",
      Select[names, MissingQ[glassData[#]] &]]; Return[$Failed]];
  refN = glassIndex[#, lambdad] & /@ refNames;
  newN = glassIndex[#, lambdad] & /@ names;
  refBefore = {1., refN[[1]], 1., refN[[2]], 1., refN[[3]]};
  refAfter = {refN[[1]], 1., refN[[2]], 1., refN[[3]], 1.};
  newBefore = {1., newN[[1]], 1., newN[[2]], 1., newN[[3]]};
  newAfter = {newN[[1]], 1., newN[[2]], 1., newN[[3]], 1.};
  powers = (refAfter - refBefore)/referenceF2Radii;
  radii = (newAfter - newBefore)/powers;
  p = <|"r" -> radii, "t" -> {3.25896, 0.99997, 2.95208},
    "g12" -> 6.00755, "g23" -> 4.75041, "stopGap" -> 0.,
    "bfl" -> 42.20778, "semiAperture" -> 5.,
    "glassNames" -> names, "targetFocalLength" -> 50.,
    "fields" -> {0., maxField/2., maxField}|>;
  base = p;
  If[targetBFL === Automatic,
    p = scaleToFocalLength[base, target],
    candidate[s_?NumericQ, q_?NumericQ] := Module[{c = scalePrescription[base, s]},
      Join[c, <|"g23" -> s q base["g23"]|>]];
    solution = Quiet@Check[FindRoot[{
       effectiveFocalLength[candidate[scale, gapScale], lambdad] == target,
       backFocalLength[candidate[scale, gapScale], lambdad] == targetBFL},
      {{scale, target/50.}, {gapScale, 1.}}, MaxIterations -> 100], $Failed];
    If[solution === $Failed,
      Print["Impossibile soddisfare simultaneamente EFL e BFL con questa forma."];
      Return[$Failed]];
    p = candidate[scale /. solution, gapScale /. solution]
  ];
  p = Join[p, <|"bfl" -> If[targetBFL === Automatic,
      backFocalLength[p, lambdad], targetBFL],
    "semiAperture" -> target/(2. fNumber),
    "targetFocalLength" -> target,
    "targetBFL" -> If[targetBFL === Automatic,
      backFocalLength[p, lambdad], targetBFL]|>];
  p
];

(* Prescrizione Cooke fornita dall'utente.
   Sequenza delle distanze dopo S1...S5: 4, 15.3, 2, 15.3, 4 mm.
   Il diaframma e posto sulla quarta superficie (stopGap = 0). *)
ClearAll[GivenCookePrescription];
GivenCookePrescription[] := <|
  "r" -> {34.23, 606.7, -52.39, 27.87, 94.22, -41.13},
  "t" -> {4., 2., 4.},
  "g12" -> 15.3, "g23" -> 15.3, "stopGap" -> 0.,
  "bfl" -> 79.8, "semiAperture" -> 10.,
  "glassNames" -> {"N-SK16", "SF2", "N-SK16"},
  "targetFocalLength" -> 100., "targetBFL" -> 79.8,
  "fields" -> {0., 5., 10.}, "objectDistance" -> Infinity|>;

(* Punto iniziale predefinito per tracciamento e ottimizzazione. *)
start = GivenCookePrescription[];

(* Intersezione esatta raggio-sfera e rifrazione vettoriale di Snell. *)
ClearAll[refractSurface];
refractSurface[ray : {p0_, u0_}, s_Association] := Module[
  {c, q, disc, roots, points, tt, point, normal, eta, cosi, k, u},
  c = {s["z"] + s["R"], 0.}; q = p0 - c;
  disc = (q . u0)^2 - (q . q - s["R"]^2);
  If[disc < 0, Return[$Failed]];
  roots = Select[{-q . u0 - Sqrt[disc], -q . u0 + Sqrt[disc]}, # > 10^-9 &];
  If[roots === {}, Return[$Failed]];
  (* Una sfera matematica ha due calotte: si sceglie l'intersezione vicina
     al vertice ottico dichiarato, non semplicemente la prima incontrata. *)
  points = p0 + # u0 & /@ roots;
  tt = First@MinimalBy[roots, Abs[(p0 + # u0)[[1]] - s["z"]] &];
  point = p0 + tt u0;
  normal = Normalize[point - c];
  If[u0 . normal > 0, normal = -normal];
  eta = s["n1"]/s["n2"]; cosi = -normal . u0;
  k = 1 - eta^2 (1 - cosi^2);
  If[k < 0, Return[$Failed]];
  u = Normalize[eta u0 + (eta cosi - Sqrt[k]) normal];
  {point, u}
];

ClearAll[propagateToZ, traceUntil, traceRay];
propagateToZ[{p_, u_}, z_?NumericQ] := {p + ((z - p[[1]])/u[[1]]) u, u};
traceUntil[ray_, surfaces_List] := Fold[
  If[#1 === $Failed, $Failed, refractSurface[#1, #2]] &, ray, surfaces];

traceRay[p_Association, fieldDeg_?NumericQ, pupil_?NumericQ,
   lambda_?NumericQ] := Module[{ss, z0 = -20., angle, y0, root, frontRay,
   beforeStop, atStop, afterStop, finalRay, imageZ},
  ss = surfaceData[p, lambda]; angle = fieldDeg Degree;
  (* Trova l'altezza iniziale che attraversa la coordinata richiesta del diaframma. *)
  root = Quiet@Check[FindRoot[
      Module[{r = traceUntil[{{z0, yy}, {Cos[angle], Sin[angle]}}, Take[ss, 4]]},
        If[r === $Failed, 10.^6, propagateToZ[r, stopZ[p]][[1, 2]] - pupil]],
      {yy, pupil - (stopZ[p] - z0) Tan[angle]}, MaxIterations -> 30], $Failed];
  If[root === $Failed, Return[$Failed]];
  y0 = yy /. root; frontRay = {{z0, y0}, {Cos[angle], Sin[angle]}};
  beforeStop = traceUntil[frontRay, Take[ss, 4]];
  If[beforeStop === $Failed, Return[$Failed]];
  atStop = propagateToZ[beforeStop, stopZ[p]];
  If[Abs[atStop[[1, 2]]] > p["semiAperture"], Return[$Failed]];
  afterStop = propagateToZ[atStop, ss[[5, "z"]] - 10^-7];
  finalRay = traceUntil[afterStop, Drop[ss, 4]];
  If[finalRay === $Failed, Return[$Failed]];
  imageZ = Last[vertices[p]] + p["bfl"];
  propagateToZ[finalRay, imageZ][[1, 2]]
];

(* Dati per la funzione di merito. Incrementare i campioni dopo la convergenza. *)
fields = {0., 7., 14.};
pupilFractions = {-1., -0.7, -0.35, 0., 0.35, 0.7, 1.};

ClearAll[spotData, merit];
spotData[p_Association] := Module[{fs = Lookup[p, "fields", fields]},
  Table[traceRay[p, f, q p["semiAperture"], lam],
    {f, fs}, {lam, wavelengths}, {q, pupilFractions}]];

merit[p_Association] := Module[
  {data, bad, rmsTerms, colorTerms, fs, target, focalTerm, bflTarget,
   actualBFL, bflTerm, focusTerm},
  fs = Lookup[p, "fields", fields];
  data = spotData[p]; bad = Count[data, $Failed, Infinity];
  If[bad > 0, Return[10.^6 bad]];
  rmsTerms = Flatten@Table[
     Variance[data[[fi, li]]], {fi, Length[fs]}, {li, Length[wavelengths]}];
  (* Spostamento cromatico del centroide, pesato moderatamente. *)
  colorTerms = Flatten@Table[
     (Mean[data[[fi, li]]] - Mean[data[[fi, 2]]])^2,
     {fi, Length[fs]}, {li, {1, 3}}];
  target = Lookup[p, "targetFocalLength", effectiveFocalLength[p, lambdad]];
  focalTerm = ((effectiveFocalLength[p, lambdad] - target)/target)^2;
  bflTarget = Lookup[p, "targetBFL", p["bfl"]];
  actualBFL = backFocalLength[p, lambdad];
  bflTerm = ((actualBFL - bflTarget)/bflTarget)^2;
  focusTerm = ((p["bfl"] - actualBFL)/bflTarget)^2;
  (* EFL e BFL sono specifiche, non aberrazioni sacrificabili. *)
  Mean[rmsTerms] + 0.5 Mean[colorTerms] +
    10.^8 focalTerm + 10.^8 bflTerm + 10.^7 focusTerm
];

ClearAll[PrescriptionReport, PrescriptionAcceptableQ];
PrescriptionReport[p_Association] := Module[
  {efl = effectiveFocalLength[p, lambdad],
   bfl = backFocalLength[p, lambdad], tf, tb},
  tf = Lookup[p, "targetFocalLength", efl];
  tb = Lookup[p, "targetBFL", bfl];
  <|"EFL" -> efl, "TargetEFL" -> tf,
    "EFLErrorPercent" -> 100. (efl - tf)/tf,
    "BFL" -> bfl, "TargetBFL" -> tb,
    "BFLErrorPercent" -> 100. (bfl - tb)/tb,
    "ImagePlane" -> p["bfl"], "Merit" -> merit[p]|>
];
PrescriptionAcceptableQ[p_Association, tolerancePercent_ : 1.] := Module[
  {report = PrescriptionReport[p]},
  Abs[report["EFLErrorPercent"]] <= tolerancePercent &&
   Abs[report["BFLErrorPercent"]] <= tolerancePercent];

(* Valutazione iniziale (puo richiedere alcuni secondi):
   initialMerit = merit[start];
   Print["Merito della prescrizione iniziale: ", initialMerit];
*)

(* Prima ottimizzazione consigliata: soli raggi e fuoco, con spessori fissi.
   DifferentialEvolution e robusto ma non rapido. Per una prova breve usare
   Method -> "NelderMead" e ridurre MaxIterations. *)
ClearAll[makePrescription, objective, objective7, constraints];
makePrescription[x_List] := Join[start, <|"r" -> x[[1 ;; 6]], "bfl" -> x[[7]]|>];
objective[x_List] /; VectorQ[x, NumericQ] := merit[makePrescription[x]];
objective7[r1_?NumericQ, r2_?NumericQ, r3_?NumericQ, r4_?NumericQ,
   r5_?NumericQ, r6_?NumericQ, bf_?NumericQ] :=
  objective[{r1, r2, r3, r4, r5, r6, bf}];

constraints[x_List] := And[
  And @@ MapThread[
    If[#2 > 0., 0.25 #2 <= #1 <= 4. #2,
      4. #2 <= #1 <= 0.25 #2] &,
    {x[[1 ;; 6]], start["r"]}],
  0.5 start["bfl"] <= x[[7]] <= 1.5 start["bfl"],
  (* Conservazione della forma delle tre lenti: il rapporto assoluto fra
     i raggi di ogni elemento puo variare al massimo di un fattore due. *)
  0.5 (start["r"][[2]]/start["r"][[1]]) <=
    x[[2]]/x[[1]] <= 2. (start["r"][[2]]/start["r"][[1]]),
  0.5 Abs[start["r"][[4]]/start["r"][[3]]] <=
    -x[[4]]/x[[3]] <= 2. Abs[start["r"][[4]]/start["r"][[3]]],
  0.5 Abs[start["r"][[6]]/start["r"][[5]]] <=
    -x[[6]]/x[[5]] <= 2. Abs[start["r"][[6]]/start["r"][[5]]]
];

x0 = Join[start["r"], {start["bfl"]}];

(* Decommentare per avviare l'ottimizzazione globale:

result = NMinimize[
  {objective7[r1, r2, r3, r4, r5, r6, bfl],
   constraints[{r1, r2, r3, r4, r5, r6, bfl}]},
  {r1, r2, r3, r4, r5, r6, bfl},
  Method -> {"DifferentialEvolution", "InitialPoints" -> {x0}},
  MaxIterations -> 250
];

bestVector = {r1, r2, r3, r4, r5, r6, bfl} /. Last[result];
best = makePrescription[bestVector];
Print["Prescrizione ottimizzata: ", best];
Print["Merito finale: ", merit[best]];

*)

(* Grafico semplice delle intercette sul piano immagine. *)
ClearAll[spotPlot];
spotPlot[p_Association] := ListPlot[
  Flatten[spotData[p], 2], PlotRange -> All,
  PlotStyle -> {Red, Darker[Green], Blue},
  AxesLabel -> {"campione", "y immagine [mm]"},
  PlotLabel -> "Intercette meridionali (campi e colori)"];

(* Dimostrazione della prescrizione iniziale. *)
ClearAll[RunCookeDemo];
RunCookeDemo[] := Module[{m, graph},
  Print["Calcolo della prescrizione iniziale in corso..."];
  m = merit[start];
  Print["Merito iniziale = ", m];
  Print["Prescrizione iniziale = ", start];
  graph = spotPlot[start];
  Print[graph];
  <|"Prescription" -> start, "Merit" -> m, "Plot" -> graph|>
];

(* Ottimizzazione completa. Puo richiedere parecchi minuti.
   Un valore 20--40 serve per una prova; 150--300 per un calcolo reale. *)
ClearAll[RunCookeOptimization];
RunCookeOptimization[maxIterations_Integer : 250] /; maxIterations > 0 := Module[
  {vars = {r1, r2, r3, r4, r5, r6, bf}, result, bestVector, best,
   initialPoint},
  Print["Ottimizzazione avviata; iterazioni massime = ", maxIterations];
  initialPoint = N@Join[start["r"], {start["bfl"]}];
  If[! TrueQ[constraints[initialPoint]],
    Print["Errore interno: il punto start non soddisfa i vincoli."];
    Return[$Failed]];
  result = NMinimize[
    {objective7[r1, r2, r3, r4, r5, r6, bf], constraints[vars]},
    vars,
    Method -> {"DifferentialEvolution", "InitialPoints" -> {initialPoint}},
    MaxIterations -> maxIterations];
  bestVector = vars /. Last[result];
  best = makePrescription[bestVector];
  Print["Valore finale della funzione di merito = ", First[result]];
  Print["Prescrizione ottimizzata = ", best];
  Print["Controllo delle specifiche = ", PrescriptionReport[best]];
  If[! PrescriptionAcceptableQ[best, 1.],
    Print["SOLUZIONE RIFIUTATA: EFL o BFL differiscono di oltre l'1%."];
    Return[<|"Status" -> "Rejected", "NMinimizeResult" -> result,
      "Prescription" -> best, "Report" -> PrescriptionReport[best]|>]];
  <|"NMinimizeResult" -> result, "Prescription" -> best,
    "Merit" -> First[result], "Report" -> PrescriptionReport[best],
    "Status" -> "Accepted"|>
];

Print["CookeTripletDesigner caricato correttamente."];
Print["Punto iniziale: EFL = ", N[effectiveFocalLength[start], 8],
  " mm; BFL = ", N[backFocalLength[start], 8], " mm; vetri = ",
  start["glassNames"]];
Print["Eseguire RunCookeDemo[] per analizzare il progetto iniziale."];
Print["Eseguire RunCookeOptimization[30] per una prima ottimizzazione breve."];
Print["Eseguire UpdateSchottCatalog[] per aggiornare il catalogo ufficiale SCHOTT."];


(* ::Title:: *)
(*Progettazione approssimata del tripletto di Cooke*)


(* ::Text:: *)
(*1. Selezionare la cella seguente e premere Maiusc+Invio per caricare il modello.*)


(* ::Input:: *)
(*Get["F:/Profilo/Documenti/ChatGPT/Tripletto di Cooke/CookeTripletDesigner.wl"]*)


(* ::Text:: *)
(*2. Selezionare la cella seguente e premere Maiusc+Invio per analizzare la prescrizione iniziale.*)


(* ::Input:: *)
(*demo=RunCookeDemo[]*)


(* ::Text:: *)
(*3. Solo dopo la prova precedente, usare questa cella per una breve ottimizzazione.*)


(* ::Input:: *)
(*risultato=RunCookeOptimization[30]*)


(* ::Text:: *)
(*La prescrizione risultante si ottiene valutando risultato["Prescription"].*)


(* ::Input:: *)
(*risultato["Prescription"]*)
