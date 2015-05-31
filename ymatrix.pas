unit ymatrix;

{$mode delphi}
//{$link cpklusolve.o}
//{$link cpklu_analyze_given.o}
//{$link cpklu_factor.o}
//{$link cpklu_refactor.o}
//{$link cpklu_tsolve.o}
//{$link cpklu_analyze.o}
//{$link cpklu_free_numeric.o}
//{$link cpklu_scale.o}
//{$link cpklu_z_stuff.o}
//{$link cpklu_defaults.o}
//{$link cpklu_free_symbolic.o}
//{$link cpklu_solve.o}
//{$link cpsz_stuff.o}
//{$link cpklu_diagnostics.o}
//{$link cpklu_kernel.o}
//{$link cpklusolve.o}
//{$link cpklu_dump.o}
//{$link cpklu_memory.o}
//{$link cpklu_sort.o}
//{$link cpklu_extract.o}
//{$link cpklu.o}
//{$link cpklusystem.o}
//{$link chello.o}
//{$link cpkluall.o}
//{$linklib c}
//In the helloU.pas file I replaced {$linklib c} with
{$linklib libmsvcrt}//widows
//{$IFDEF MSWINDOWS}
//  {$linklib libmsvcrt}
//{$ELSE}
//  {$linklib c}
//{$ENDIF}
{$linklib m}    //for math
//{$linklib amd}
//{$linklib btf}
//{$linklib colamd}
//{$linklib cpklu}
interface

uses
  Classes, SysUtils,uComplex,ctypes;

{Options for building Y matrix}
CONST
      SERIESONLY = 1;
      WHOLEMATRIX = 2;

TYPE
  EEsolv32Problem = class(Exception);
  //var
 // balreadydone:boolean;

PROCEDURE BuildYMatrix(BuildOption :Integer; AllocateVI:Boolean);
PROCEDURE BuildRealYMatrix(BuildOption :Integer; AllocateVI:Boolean);
PROCEDURE ResetSparseMatrix(var hYlocal:LongWord; size:integer);
PROCEDURE ResetRealSparseMatrix(var hRealYlocal:LongWord; size:integer);
PROCEDURE InitializeNodeVbase;

Function CheckYMatrixforZeroes:String;

{Declare FUNCTIONs in KLUSolve DLL}

//{ $INCLUDE Esolv32Declarations.pas}
{$INCLUDE KLUSolveDeclarations.pas}


implementation

Uses DSSGlobals, Circuit, CktElement, Utilities;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
PROCEDURE ReCalcAllYPrims;

VAR
   pElem:TDSSCktElement;

Begin

  WITH ActiveCircuit Do
  Begin
     If LogEvents Then LogThisEvent('Recalc All Yprims');
     pElem := CktElements.First;
     WHILE pElem<>nil Do Begin
       pElem.CalcYPrim;
       pElem := CktElements.Next;
     End;
  End;

End;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
PROCEDURE ReCalcInvalidYPrims;
{Recalc YPrims only for those circuit elements that have had changes since last
 solution}
VAR
   pElem:TDSSCktElement;

Begin

  WITH ActiveCircuit Do
  Begin
     If LogEvents Then LogThisEvent('Recalc Invalid Yprims');
     pElem := CktElements.First;
     WHILE pElem<>nil Do
     Begin
       WITH pElem Do
       IF YprimInvalid THEN CalcYPrim;
       pElem := CktElements.Next;
     End;
  End;

End;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
PROCEDURE ResetSparseMatrix(var hYlocal:LongWord; size:integer);

var
   hytest: ctypes.cint32;
   hRealYlocal1:LongWord;
   hRealYlocal2:LongWord;
Begin
 // writeln('hylocal size:');
  IF hYlocal<>0 THEN Begin
      //IF DeleteSparseSet(hYlocal) < 1  {Get rid of existing one beFore making a new one}
      //THEN Raise EEsolv32Problem.Create('Error Deleting System Y Matrix in ResetSparseMatrix. Problem with Sparse matrix solver.');

      hYlocal := 0;
  End;
  hrealylocal1:=0;
  //writeln(100);
  // Make a new sparse set
  hRealYlocal2:=0;
  //hYlocal := NewSparseSet(Size);
  //ResetRealSparseMatrix(hRealYlocal2,size*2);
  //ResetRealSparseMatrix(hRealYlocal,size*2);
  //writeln('finish complex matrix!');
  // hYtest :=NewSparseSettest(12);
  //writeln(hy);
  If hYlocal<1 THEN Begin   // Raise and exception
     Raise EEsolv32Problem.Create('Error Creating System Y Matrix. Problem WITH Sparse matrix solver.');
  End;
 // hRealYlocal1:= NewRealSparseSet(2*Size);
  //writeln('1');

End;

PROCEDURE ResetRealSparseMatrix(var hRealYlocal:LongWord; size:integer);

var
   hytest: ctypes.cint32;
Begin
//  writeln('hRealy size:');
//  writeln(Size);
  IF hRealYlocal<>0 THEN Begin
      //IF DeleteRealSparseSet(hRealYlocal) < 1  {Get rid of existing one beFore making a new one}
      //THEN Raise EEsolv32Problem.Create('Error Deleting System Y Matrix in ResetSparseMatrix. Problem with Sparse matrix solver.');

      hRealYlocal := 0;
  End;

  //writeln(100);
  // Make a new sparse set
  //  writeln('finish!');
  //hRealYlocal := NewRealSparseSet(Size);
  //hRealYlocal:= NewSparseSet(2*Size);
// writeln('finish!');
  // hYtest :=NewSparseSettest(12);
  //writeln(hy);
  If hRealYlocal<1 THEN Begin   // Raise and exception
     Raise EEsolv32Problem.Create('Error Creating System Y Matrix. Problem WITH Sparse matrix solver.');
  End;
End;

Procedure InitializeNodeVbase;

Var
   i: Integer;

Begin

    WITH ActiveCircuit, Solution  Do Begin
       FOR i := 1 to NumNodes Do
         WITH MapNodeToBus^[i]  Do
         Begin
              NodeVbase^[i] := Buses^[BusRef].kvbase * 1000.0;
         End;
         VoltageBaseChanged := FALSE;
    End;
End;

PROCEDURE BuildYMatrix(BuildOption :Integer; AllocateVI:Boolean);

{Builds designated Y matrix for system and allocates solution arrays}

VAR
   YMatrixsize:Integer;
   CmatArray:pComplexArray;
   pElem:TDSSCktElement;
   htest:longword;

   //{****} FTrace: TextFile;


Begin

  //{****} AssignFile(Ftrace, 'YmatrixTrace.txt');
  //{****} Rewrite(FTrace);
  htest:=0;

   CmatArray := Nil;
   // new function to log KLUSolve.DLL function calls
   // SetLogFile ('KLU_Log.txt', 1);
   WITH ActiveCircuit, ActiveCircuit.Solution  Do Begin

     If PreserveNodeVoltages Then UpdateVBus; // Update voltage values stored with Bus object

     // the following re counts the number of buses and resets meter zones and feeders
     // If radial but systemNodeMap not set then init for radial got skipped due to script sequence
     IF (BusNameRedefined) THEN ReProcessBusDefs;      // This changes the node references into the system Y matrix!!

     YMatrixSize := NumNodes;

     Case BuildOption of
         WHOLEMATRIX: begin
           //ResetSparseMatrix (hYsystem, YMatrixSize);
           //ResetRealSparseMatrix (hRealYsystem, 2*YMatrixSize);
           //BuildRealYMatrix(WHOLEMATRIX,True);
           //ResetRealSparseMatrix(htest,7);
           hY := hYsystem;
           hRealY:=hRealYsystem;
         end;
         SERIESONLY: begin
           //ResetSparseMatrix (hYseries, YMatrixSize);
           //ResetRealSparseMatrix (hRealYseries, 2*YMatrixSize);
           hY := hYSeries;
           hRealY:=hRealYseries;
         end;
     End;


     // tune up the Yprims if necessary
     IF  (FrequencyChanged) THEN ReCalcAllYPrims
                            ELSE ReCalcInvalidYPrims;


     if SolutionAbort then  Begin
       DoSimpleMsg('Y matrix build aborted due to error in primitive Y calculations.', 11001);
       Exit;  // Some problem occured building Yprims
     End;


     FrequencyChanged := FALSE;

     If LogEvents Then  Case BuildOption of
        WHOLEMATRIX: LogThisEvent('Building Whole Y Matrix');
        SERIESONLY: LogThisEvent('Building Series Y Matrix');
     End;

          // Add in Yprims for all devices
     pElem := CktElements.First;
     WHILE pElem <> Nil Do
       Begin
         WITH pElem Do
         IF  (Enabled) THEN Begin          // Add stuff only if enabled
           Case BuildOption of
              WHOLEMATRIX : CmatArray := GetYPrimValues(ALL_YPRIM);
              SERIESONLY:   CmatArray := GetYPrimValues(SERIES)
           End;
           // new function adding primitive Y matrix to KLU system Y matrix
           if CMatArray <> Nil then
           //writeln('formY');
              //if AddPrimitiveMatrix (hY, Yorder, @NodeRef[1], @CMatArray[1]) < 1 then
              writeln('node index out of range adding to system y matrix');
              //if AddRealPrimitiveComplexMatrix (hRealY, Yorder, @NodeRef[1], @CMatArray[1]) < 1 then
              writeln('node index out of range adding to system y matrix')
              //   Raise EEsolv32Problem.Create('Node index out of range adding to System Y Matrix')
         End;   // If Enabled
         pElem := CktElements.Next;
       End;
      //ResetSparseMatrix(htest,7);
     //{****} CloseFile(Ftrace);
     //{****} FireOffEditor(  'YmatrixTrace.txt');
     //testklurealsystem(hY);
     //testklurealsystem(hRealY);

     // Allocate voltage and current vectors if requested
     IF   AllocateVI
     THEN Begin
         If LogEvents Then LogThisEvent('ReAllocating Solution Arrays');
         ReAllocMem(NodeV,    SizeOf(NodeV^[1])* (NumNodes+1)*2); // Allocate System Voltage array - allow for zero element
         DoubleNodeV:=nil;
         ReAllocMem(DoubleNodeV,    SizeOf(DoubleNodeV^[0])*(NumNodes+1)*2); // Allocate System Voltage array - allow for zero element
         NodeV^[0] := CZERO;
         DoubleNodeV^[0]:=0.0;
         DoubleNodeV^[1]:=0.0;
         ReAllocMem(Currents, SizeOf(Currents^[1]) * (NumNodes+1)*2); // Allocate System current array
         DoubleCurrents:=nil;
         ReAllocMem(DoubleCurrents, SizeOf(DoubleCurrents^[0]) * (NumNodes+1)*2); // Allocate System current array
         //ReAllocMem(Currents, SizeOf(Currents^[1]) * (NumNodes+1)); // Allocate System current array
         ReAllocMem(AuxCurrents, SizeOf(AuxCurrents^[1]) * NumNodes); // Allocate System current array
         IF (VMagSaved  <> Nil) THEN ReallocMem(VMagSaved, 0);
         IF (ErrorSaved <> Nil) THEN ReallocMem(ErrorSaved, 0);
         IF (NodeVBase  <> Nil) THEN ReallocMem(NodeVBase, 0);
         VMagSaved      := AllocMem(Sizeof(VMagSaved^[1])  * NumNodes);  // zero fill
         ErrorSaved     := AllocMem(Sizeof(ErrorSaved^[1]) * NumNodes);  // zero fill
         NodeVBase      := AllocMem(Sizeof(NodeVBase^[1]) * NumNodes);  // zero fill
         InitializeNodeVbase;

     End;
     //testklurealsystem(hY);
     //testklurealsystem(hRealY);

     Case BuildOption of
          WHOLEMATRIX: Begin
                           SeriesYInvalid := True;  // Indicate that the Series matrix may not match
                           SystemYChanged := False;
                       End;
          SERIESONLY: SeriesYInvalid := False;  // SystemYChange unchanged
     End;

    // Deleted RCD only done now on mode change
    // SolutionInitialized := False;  //Require initialization of voltages if Y changed

    If PreserveNodeVoltages Then RestoreNodeVfromVbus;
     //testklurealsystem(hY);
     //testklurealsystem(hRealY);
   End;
End;

PROCEDURE BuildRealYMatrix(BuildOption :Integer; AllocateVI:Boolean);

{Builds designated Y matrix for system and allocates solution arrays}

VAR
   YMatrixsize:Integer;
   CmatArray:pComplexArray;
   pElem:TDSSCktElement;


   //{****} FTrace: TextFile;


Begin

  //{****} AssignFile(Ftrace, 'YmatrixTrace.txt');
  //{****} Rewrite(FTrace);

   CmatArray := Nil;
   // new function to log KLUSolve.DLL function calls
   // SetLogFile ('KLU_Log.txt', 1);
   WITH ActiveCircuit, ActiveCircuit.Solution  Do Begin

     If PreserveNodeVoltages Then UpdateVBus; // Update voltage values stored with Bus object

     // the following re counts the number of buses and resets meter zones and feeders
     // If radial but systemNodeMap not set then init for radial got skipped due to script sequence
     IF (BusNameRedefined) THEN ReProcessBusDefs;      // This changes the node references into the system Y matrix!!

     YMatrixSize := 2*NumNodes;

     Case BuildOption of
         WHOLEMATRIX: begin
           //if hRealY<>0    then     hRealYsystem:=hRealY
           //else
           //begin
           //ResetRealSparseMatrix (hRealYsystem, YMatrixSize);
           hRealY := hRealYsystem;
           //balreadydone:=True;
           //end;
         end;
         SERIESONLY: begin
           //if hRealY<>0
           //then
           //hRealYSeries:=hRealY
           //else
           //begin
           //ResetRealSparseMatrix (hRealYseries, YMatrixSize);
           hRealY := hRealYSeries;
           //balreadydone:=True;
           //end;
           end;
     End;

     // tune up the Yprims if necessary
     IF  (FrequencyChanged) THEN ReCalcAllYPrims
                            ELSE ReCalcInvalidYPrims;

     if SolutionAbort then  Begin
       DoSimpleMsg('Y matrix build aborted due to error in primitive Y calculations.', 11001);
       Exit;  // Some problem occured building Yprims
     End;


     FrequencyChanged := FALSE;

     If LogEvents Then  Case BuildOption of
        WHOLEMATRIX: LogThisEvent('Building Whole Y Matrix');
        SERIESONLY: LogThisEvent('Building Series Y Matrix');
     End;
          // Add in Yprims for all devices
     pElem := CktElements.First;
     WHILE pElem <> Nil Do
       Begin
         WITH pElem Do
         IF  (Enabled) THEN Begin          // Add stuff only if enabled
           Case BuildOption of
              WHOLEMATRIX : CmatArray := GetYPrimValues(ALL_YPRIM);
              SERIESONLY:   CmatArray := GetYPrimValues(SERIES)
           End;
           // new function adding primitive Y matrix to KLU system Y matrix
           if CMatArray <> Nil then
           //writeln('formY');
              //if AddRealPrimitiveComplexMatrix (hRealY, Yorder, @NodeRef[1], @CMatArray[1]) < 1 then
              writeln('node index out of range adding to system y matrix')
              //   Raise EEsolv32Problem.Create('Node index out of range adding to System Y Matrix')
         End;   // If Enabled
         pElem := CktElements.Next;
       End;

     //{****} CloseFile(Ftrace);
     //{****} FireOffEditor(  'YmatrixTrace.txt');

     // Allocate voltage and current vectors if requested
     IF   AllocateVI
     THEN Begin
         If LogEvents Then LogThisEvent('ReAllocating Solution Arrays');
         ReAllocMem(NodeV,    SizeOf(NodeV^[1])        * (NumNodes+1)); // Allocate System Voltage array - allow for zero element
         ReAllocMem(DoubleNodeV,    SizeOf(DoubleNodeV^[1])        * (NumNodes+1)*2); // Allocate System Voltage array - allow for zero element
         NodeV^[0] := CZERO;
         ReAllocMem(Currents, SizeOf(Currents^[1]) * (NumNodes+1)); // Allocate System current array
         ReAllocMem(DoubleCurrents, SizeOf(DoubleCurrents^[1]) * (NumNodes+1)*2); // Allocate System current array

         ReAllocMem(AuxCurrents, SizeOf(AuxCurrents^[1]) * NumNodes); // Allocate System current array
         IF (VMagSaved  <> Nil) THEN ReallocMem(VMagSaved, 0);
         IF (ErrorSaved <> Nil) THEN ReallocMem(ErrorSaved, 0);
         IF (NodeVBase  <> Nil) THEN ReallocMem(NodeVBase, 0);
         VMagSaved      := AllocMem(Sizeof(VMagSaved^[1])  * NumNodes);  // zero fill
         ErrorSaved     := AllocMem(Sizeof(ErrorSaved^[1]) * NumNodes);  // zero fill
         NodeVBase      := AllocMem(Sizeof(NodeVBase^[1]) * NumNodes);  // zero fill
         InitializeNodeVbase;

     End;

     Case BuildOption of
          WHOLEMATRIX: Begin
                           SeriesYInvalid := True;  // Indicate that the Series matrix may not match
                           SystemYChanged := False;
                       End;
          SERIESONLY: SeriesYInvalid := False;  // SystemYChange unchanged
     End;

    // Deleted RCD only done now on mode change
    // SolutionInitialized := False;  //Require initialization of voltages if Y changed

    If PreserveNodeVoltages Then RestoreNodeVfromVbus;

   End;
End;

// leave the call to GetMatrixElement, but add more diagnostics
Function CheckYMatrixforZeroes:String;

Var
    i                           :LongWord;
    c                           :Complex;
    hY                          :LongWord;
    sCol                        :LongWord;
    nIslands, iCount, iFirst, p :LongWord;
    Cliques                     :array of LongWord;
Begin

  Result := '';
  With ActiveCircuit Do begin
    hY := Solution.hY;
    For i := 1 to Numnodes Do Begin
       //GetMatrixElement(hY, i, i, @c);
       If Cabs(C)=0.0 Then With MapNodeToBus^[i] Do Begin
           Result := Result + Format('%sZero diagonal for bus %s, node %d',[CRLF, BusList.Get(Busref), NodeNum]);
       End;
    End;

    // new diagnostics
    //GetSingularCol (hY, @sCol); // returns a 1-based node number
    if sCol > 0 then With MapNodeToBus^[sCol] Do Begin
      Result := Result + Format('%sMatrix singularity at bus %s, node %d',[CRLF, BusList.Get(Busref), sCol]);
    end;

    SetLength (Cliques, NumNodes);
    //nIslands := FindIslands (hY, NumNodes, @Cliques[0]);
    if nIslands > 1 then begin
      Result := Result + Format('%sFound %d electrical islands:', [CRLF, nIslands]);
      for i:= 1 to nIslands do begin
        iCount := 0;
        iFirst := 0;
        for p := 0 to NumNodes - 1 do begin
          if Cliques[p] = i then begin
            Inc (iCount, 1);
            if iFirst = 0 then iFirst := p+1;
          end;
        end;
        With MapNodeToBus^[iFirst] Do Begin
          Result := Result + Format('%s  #%d has %d nodes, including bus %s (node %d)',[CRLF, i, iCount, BusList.Get(Busref), iFirst]);
        end;
      end;
    end;
  End;

End;

{FUNCTION NewSparseSet(nBus:LongWord):LongWord;//cdecl;external;
begin
  writeln('1');
  end;}
// return 1 for success, 0 for invalid handle
{FUNCTION DeleteSparseSet(id:LongWord):LongWord;//cdecl;external;
begin
  writeln('1');
  end;
// return 1 for success, 2 for singular, 0 for invalid handle
// factors matrix if needed
FUNCTION SolveSparseSet(id:LongWord; x,b:pComplexArray):LongWord;//cdecl;external;
begin
  writeln('1');
  end;
// return 1 for success, 0 for invalid handle
FUNCTION ZeroSparseSet(id:LongWord):LongWord;//cdecl;external;
begin
  writeln('1');
  end;
// return 1 for success, 2 for singular, 0 for invalid handle
// FactorSparseMatrix does no extra work if the factoring was done previously
FUNCTION FactorSparseMatrix(id:LongWord):LongWord;//cdecl;external;
begin
  writeln('1');
  end;
// These "Get" functions for matrix information all return 1 for success, 0 for invalid handle
// Res is the matrix order (number of nodes)
FUNCTION GetSize(id:LongWord; Res: pLongWord):LongWord;        //cdecl;external;
begin
  writeln('1');
  end;
// the following function results are not known prior to factoring
// Res is the number of floating point operations to factor
FUNCTION GetFlops(id:LongWord; Res: pDouble):LongWord;        //cdecl;external;
begin
  writeln('1');
  end;
// Res is number of non-zero entries in the original matrix
FUNCTION GetNNZ(id:LongWord; Res: pLongWord):LongWord;        //cdecl;external;
begin
  writeln('1');
  end;
// Res is the number of non-zero entries in factored matrix
FUNCTION GetSparseNNZ(id:LongWord; Res: pLongWord):LongWord;        //cdecl;external;
begin
  writeln('1');
  end;
// Res is a column number corresponding to a singularity, or 0 if not singular
FUNCTION GetSingularCol(id:LongWord; Res: pLongWord):LongWord;        //cdecl;external;
begin
  writeln('1');
  end;
// Res is the pivot element growth factor
FUNCTION GetRGrowth(id:LongWord; Res: pDouble):LongWord;        //cdecl;external;
begin
  writeln('1');
  end;
// Res is aquick estimate of the reciprocal of condition number
FUNCTION GetRCond(id:LongWord; Res: pDouble):LongWord;        //cdecl;external;
begin
  writeln('1');
  end;
// Res is a more accurate estimate of condition number
FUNCTION GetCondEst(id:LongWord; Res: pDouble):LongWord;        //cdecl;external;
begin
  writeln('1');
  end;
// return 1 for success, 0 for invalid handle or a node number out of range
FUNCTION AddPrimitiveMatrix(id, nOrder:LongWord; Nodes: pLongWord; Mat: pComplex):LongWord;        //cdecl;external;
begin
  writeln('1');
  end;
// Action = 0 (close), 1 (rewrite) or 2 (append)
FUNCTION SetLogFile(Path: pChar; Action:LongWord):LongWord;        //cdecl;external;
begin
  writeln('1');
  end;
// fill sparse matrix in compressed column form
// return 1 for success, 0 for invalid handle, 2 for invalid array sizes
// pColP must be of length nColP == nBus + 1
// pRowIdx and pMat of length nNZ, which
//    must be at least the value returned by GetNNZ
FUNCTION GetCompressedMatrix(id, nColP, nNZ:LongWord; pColP, pRowIdx: pLongWord; Mat: pComplex):LongWord; //cdecl;external;
begin
  writeln('1');
  end;
// fill sparse matrix in triplet form
// return 1 for success, 0 for invalid handle, 2 for invalid array sizes
// pRows, pCols, and Mat must all be of length nNZ
FUNCTION GetTripletMatrix(id, nNZ:LongWord; pRows, pCols: pLongWord; Mat: pComplex):LongWord; //cdecl;external;
begin
  writeln('1');
  end;
// returns number of islands >= 1 by graph traversal
// pNodes contains the island number for each node
FUNCTION FindIslands(id, nOrder:LongWord; pNodes: pLongWord):LongWord;        //cdecl;external;
begin
  writeln('1');
  end;
// AddMatrixElement is deprecated, use AddPrimitiveMatrix instead
FUNCTION AddMatrixElement(id:LongWord; i,j:LongWord; Value:pComplex):LongWord;   //cdecl;external;
begin
  writeln('1');
  end;
// GetMatrixElement is deprecated, use GetCompressedMatrix or GetTripletMatrix
FUNCTION GetMatrixElement(id:LongWord; i,j:LongWord; Value:pComplex):LongWord;   //cdecl;external;
begin
  writeln('1');
  end;
     }

end.

