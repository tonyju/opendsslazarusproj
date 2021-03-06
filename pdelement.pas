unit PDElement;

{$mode delphi}

interface

uses
  Classes, SysUtils,CktElement, ucomplex, ucmatrix, DSSClass, MeterElement;

TYPE

   TPDElement = class(TDSSCktElement)
     private
       FUNCTION Get_ExcessKVANorm (idxTerm:Integer):Complex;
       FUNCTION Get_ExcessKVAEmerg(idxTerm:Integer):Complex;

     public

       NormAmps,
       EmergAmps,
       FaultRate,  // annual faults per year
       PctPerm,    // percent of faults that are permanent in this element
       Lambda,    // net failure rate for this branch
       AccumulatedLambda,  // accumulated failure rate for this branch
       HrsToRepair       : Double;
       FromTerminal,
       ToTerminal        : Integer;  // Set by Meter zone for radial feeder
       IsShunt           : Boolean;

       NumCustomers      : Integer;
       TotalCustomers    : Integer;
       CustWeight        : Double; // Weighting factor for customers on this elemebt

       ParentPDElement   : TPDElement;

       MeterObj,                     {Upline energymeter}
       SensorObj   : TMeterElement; // Upline Sensor for this element  for allocation and estimation

       Overload_UE,
       OverLoad_EEN  :double;  // Indicate amount of branch overload

       constructor Create(ParClass:TDSSClass);
       destructor Destroy; override;

       PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
       PROCEDURE GetCurrents(Curr: pComplexArray); Override; // Get present values of terminal

       PROCEDURE CalcLambda; virtual;  // Calc failure rates for section and buses
       PROCEDURE AccumLambda;
       PROCEDURE CalcNum_Int;  // Calc Number of Interruptions in forward sweep
       PROCEDURE CalcN_Lambda;
       PROCEDURE ZeroReliabilityAccums; // Zero out reliability accumulators

       Property ExcesskVANorm[idxTerm:Integer] :Complex Read Get_ExcesskVANorm;
       Property ExcesskVAEmerg[idxTerm:Integer]:Complex Read Get_ExcesskVAEmerg;

   end;


implementation

USES
    DSSClassDefs, DSSGlobals, Bus;

{---------Summing Utility proc-------}
procedure accumsum(var a : Double; b : Double); Inline;
Begin  a := a + b; End;
{------------------------------------}

procedure TPDElement.AccumLambda;

Var
    FromBus : TDSSBus;

begin

    WITH ActiveCircuit Do Begin
        If FromTerminal = 2 Then Toterminal := 1 Else ToTerminal := 2;

        {Get Lambda for TO bus and add it to this section failure rate}
        AccumulatedLambda := Buses^[Terminals^[ToTerminal].BusRef].Lambda + Lambda;
        FromBus :=   Buses^[Terminals^[FromTerminal].BusRef];
        FromBus.TotalNumCustomers :=  FromBus.TotalNumCustomers + TotalCustomers;
        {Compute accumulated to FROM Bus; if a fault interrupter, assume it isolates all downline faults}
        If NOT HasOcpDevice Then Begin
            // accumlate it to FROM bus
            accumsum(FromBus.Lambda, AccumulatedLambda);
        End;
    End;

end;

procedure TPDElement.CalcLambda;   {Virtual function  -- LINE is different, for one}

begin
      {Default base algorithm for radial fault rate calculation}
      {May be overridden by specific device class behavior}

      Lambda := Faultrate * pctperm * 0.01;

end;

procedure TPDElement.CalcN_Lambda;
Var
   FromBus : TDSSBus;
begin
     FromBus := ActiveCircuit.Buses^[Terminals^[FromTerminal].BusRef];
     WITH  FromBus Do Begin
         accumsum(CustInterrupts, Num_Interrupt * TotalCustomers);
(****
     WriteDLLDebugfile(Format('%s.%s, Bus = %s, CustInterrupt= %.11g, Num_Interrupt= %.11g, TotalCustomers= %d, TotalNumCustomers= %d ',
                              [Self.ParentClass.Name, Self.Name, ActiveCircuit.Buslist.Get(Terminals^[FromTerminal].BusRef), CustInterrupts, Num_Interrupt, TotalCustomers, TotalNumCustomers  ]));
*)
    End;
end;

procedure TPDElement.CalcNum_Int;
begin

    With ActiveCircuit Do
    Begin
        If FromTerminal = 2 Then Toterminal := 1 Else ToTerminal := 2;
        // If no interrupting device then the downline bus will have the same num of interruptions
        With Buses^[Terminals^[ToTerminal].BusRef] Do Begin
            Num_Interrupt  :=  Buses^[Terminals^[FromTerminal].BusRef].Num_Interrupt;

            // If Interrupting device (on FROM side)then downline will have additional interruptions
            If HasOCPDevice Then Begin
                accumsum(Num_Interrupt, AccumulatedLambda);
            End;
        End;
    End;

end;

Constructor TPDElement.Create(ParClass:TDSSClass);
Begin
    Inherited Create(ParClass);

    IsShunt          := FALSE;

    FromTerminal     := 1;
    NumCustomers     := 0;
    TotalCustomers   := 0;
    AccumulatedLambda := 0.0;
    SensorObj         := NIL;
    MeterObj          := NIL;
    ParentPDElement   := NIL;
    DSSObjType        := PD_ELEMENT;
End;

destructor TPDElement.Destroy;
Begin

    Inherited Destroy;
End;

PROCEDURE TPDElement.GetCurrents(Curr: pComplexArray);
VAR
   i:Integer;
Begin
  TRY

   If Enabled Then
   Begin

     WITH ActiveCircuit.Solution DO
     FOR i := 1 TO Yorder DO Vterminal^[i] := NodeV^[NodeRef^[i]];

     YPrim.MVMult(Curr, Vterminal);
  End
  Else For i := 1 to Yorder Do Curr^[i] := cZero;

  EXCEPT
    On E: Exception Do DoErrorMsg(('Trying to Get Currents for Element: ' + Name + '.'), E.Message,
        'Has the circuit been solved?', 660);
  End;

End;

//- - - - - - - - - - - - - - - - - - - - - -
FUNCTION TPDElement.Get_ExcessKVANorm(idxTerm:Integer):Complex;

VAR
   Factor:Double;
   kVA :Complex;
Begin

     IF (NormAmps = 0.0) OR NOT Enabled   THEN Begin
          OverLoad_EEN := 0.0;  // bug fixed 1/10/00
          Result := cZero;
          Exit;
     End;

     kVA    := CmulReal(Power[idxTerm], 0.001);  // Also forces computation of Current into Itemp
     Factor := (MaxTerminalOneIMag/NormAmps - 1.0);
     IF    (Factor > 0.0) THEN  Begin
        OverLoad_EEN := Factor;
        Factor := 1.0 - 1.0/(Factor + 1.0);   // To get factor
        Result := CmulReal(kVA, Factor) ;
     End  ELSE Begin
         OverLoad_EEN := 0.0;
         Result := cZero;
     End;
{**********DEBUG CODE: Use DLL Debug file  ***}
{****    WriteDLLDebugFile(Format('%s.%s: Terminal=%u Factor=%.7g kW=%.7g kvar=%.7g Normamps=%.7g Overload_EEN=%.7g Result=%.7g +j %.7g ',
    [parentclass.Name, name, ActiveTerminalIdx, Factor, kVA.re, kVA.im, NormAmps, Overload_EEN, Result.re, Result.im ]));
*}
End;

//- - - - - - - - - - - - - - - - - - - - - -
FUNCTION TPDElement.Get_ExcessKVAEmerg(idxTerm:Integer):Complex;
VAR
   Factor:Double;
   kVA :Complex;
Begin
     IF (EmergAmps=0.0) OR NOT Enabled
     THEN Begin
          Overload_UE := 0.0;  // bug fixed 1/10/00
          Result := cZero;
          Exit;
     End;

     kVA := CmulReal(Power[idxTerm], 0.001);  // Also forces computation of Current into Itemp

     Factor := (MaxTerminalOneIMag/EmergAmps-1.0);
     IF    Factor > 0.0
     THEN  Begin
        Overload_UE := Factor;
        Factor := 1.0 - 1.0/(Factor + 1.0);  // To get Excess
        Result := CmulReal(kVA, Factor);
     End
     ELSE Begin
         Overload_UE := 0.0;
         Result := cZero;
     End;

End;

procedure TPDElement.InitPropertyValues(ArrayOffset: Integer);
begin


   PropertyValue[ArrayOffset + 1] := '400';  //Normamps
   PropertyValue[ArrayOffset + 2] := '600';  //emergamps
   PropertyValue[ArrayOffset + 3] := '0.1';  //Fault rate
   PropertyValue[ArrayOffset + 4] := '20';   // Pct Perm
   PropertyValue[ArrayOffset + 5] := '3';    // Hrs to repair

  inherited initPropertyValues(ArrayOffset + 5);

end;


procedure TPDElement.ZeroReliabilityAccums;
Var
   FromBus : TDSSBus;

begin
     FromBus := ActiveCircuit.Buses^[Terminals^[FromTerminal].BusRef];
     WITH  FromBus Do Begin
          CustInterrupts := 0.0;
          Lambda         := 0.0;
          TotalNumCustomers   := 0;
          CustDurations  := 0.0;
          Num_Interrupt  := 0.0;
     End;

end;

end.

