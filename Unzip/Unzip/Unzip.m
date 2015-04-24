(* Mathematica Package *)

(* :Title: Unzip *)

(* :Context: Unzip`*)

(* :Author:
        Rolf Mertig 
        GluonVision GmbH
        http://www.gluonvision.com
*)

(* :Package Version: 1.1 *)

(* :Mathematica Versions: 7 - 10 *)

(* :License: LGPL *)

(* :Copyright: Rolf Mertig, 2002 - 2015.  *)

(* :Installation:

   This package can be invoked without local installation by 
   Import["http://github.com/rolfmertig/Unzip/blob/master/Unzip/Unzip.m"] 
   or
   Import["http://www.mertig.com/mathdepot/Unzip.m"] 

*)

(* :Discussion:
   Unzip["file.zip", "localDir"] 
   unzips file.zip to localDir using JLink
   
   Unzip is similar to the Mathematica ExtractArchive function, except that Unzip
   prints the file names (using Print or PrintTemporary) while unzipping
   
*)
	

(* ::Title:: *)

BeginPackage["Unzip`",{"JLink`"}]

Unprotect[Unzip];
ClearAll[Unzip];
  
Unzip::usage = "Unzip[zipfile, dir] unzips zipfile to dir. The directory dir should exist.
Unzip[zipfile] is the same as Unzip[zipfile, $TemporaryDirectory].
Unzip has an option Print. If set to PrintTemporary or Print, each extracted file is printed.";

Begin["`Private`"];

Options[Unzip] = {CharacterEncoding :> $CharacterEncoding, Print -> PrintTemporary };

Unzip[zipfilein_String, opts:OptionsPattern[]] :=
    Unzip[zipfilein, $TemporaryDirectory, opts];

(* write this package such that it can also be saved as .mx file and still work*)
Unzip[zipfilein_String?FileExistsQ, dir_String?FileExistsQ, OptionsPattern[]] :=
    Block[ {javaBlock, javaNew, mkdirs,getName,isDirectory,
    getInputStream,read,write,close,size,entries,nextElement},
        Needs["JLink`"];
        Symbol["JLink`InstallJava"][];
        {javaBlock, javaNew} = Symbol /@ {"JLink`JavaBlock", "JLink`JavaNew" };
        javaBlock[
        Module[ {enum, exdir = dir, saveEntry, createdirs, startdir, zf, buf, zipfile, printFun, targets, target, len},
            zipfile = If[ DirectoryName[zipfilein] === "",
                          FileNameJoin[{Directory[],zipfilein}],
                          zipfilein
                      ];
            buf = javaNew["[B", 8192]; (* ] *)
            startdir = Directory[];
            createdirs = Function[di,If[ FileNames[di]==={},
                                         JavaNew["java.io.File", di][mkdirs[]]
                                     ] ];
            If[ startdir =!= dir, (* created dir if needed and do SetDirectory[dir] *)
                createdirs[dir];
                exdir = SetDirectory[dir]
            ];
            saveEntry[zipfi_, zipentry_, characterencoding_: $CharacterEncoding] :=
                javaBlock[
                 Block[ {bos, fi, fos, numRead, stream, outStream, fromcharcode, topdirle},
                     fi = zipentry[getName[]];
                     If[ zipentry[isDirectory[]],
                         createdirs[StringJoin[dir, $PathnameSeparator, fi]],
                         stream = 
                          javaNew["java.io.BufferedInputStream", 
                           zipfi[getInputStream[zipentry]]];
                         outStream = 
                          javaNew["java.io.BufferedOutputStream", 
                           javaNew["java.io.FileOutputStream", 
                                          StringJoin[dir, "/", fi]]];
                         While[(numRead = stream[read[buf]]) > 0, 
                                      outStream[write[buf, 0, numRead]]];
                         stream[close[]];
                         outStream[close[]];
                     ]
                 ]];
            zf = javaNew["java.util.zip.ZipFile", zipfile];
            len = zf[size[]];
            enum = zf[entries[]];
            printFun = OptionValue[Print] /. Options[Unzip];
            targets = Table[enum[nextElement[]],{len}];
            (* TODO: it should be possible to paralleze this *)
            Do[
                   If[ printFun=!= False,
                       If[ printFun === True,
                           printFun = PrintTemporary
                       ];
                       printFun[StringJoin["extracting: ", exdir, 
                       $PathnameSeparator, 
                       StringReplace[target[getName[]], "/" -> $PathnameSeparator]]]
                   ];
                   saveEntry[zf, target],
               {target, targets}
            ];
            zf @ close[];
            If[ startdir =!= dir, (* set Directory[] back to startdir *)
                SetDirectory[startdir]
            ];
            dir
        ]]
    ];
    
SetAttributes[Unzip, ReadProtected];
Protect[Unzip];   

End[];

EndPackage[]