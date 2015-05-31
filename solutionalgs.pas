unit solutionalgs;

{$mode delphi}

interface

uses
  Classes, SysUtils;

function SolveMonte1: integer;   // Solve Monte Carlo Solution
function SolveMonte2: integer;   // Solve Monte Carlo Solution
function SolveMonte3: integer;   // Solve Monte Carlo Solution
function SolveMonteFault: integer;  // Solve Monte Carlo Fault Study
function SolveFaultStudy: integer;  // Full Fault Study
function SolveDaily: integer;    // Solve Following Daily Cycle
function SolvePeakDay: integer;   // Solve Following Daily Cycle at peak load
function SolveYearly: integer;   // Solve Following Yearly Cycle
function SolveDuty: integer;     // Solve Following Duty Cycle
function SolveDynamic: integer;  // Solve Dynamics
function SolveLD1: integer;      // solve Load-Duration Curve, 1
function SolveLD2: integer;      // solve Load-Duration Curve, 2
function SolveHarmonic: integer;
function SolveGeneralTime: integer;

procedure ComputeYsc(iB: integer);
procedure ComputeAllYsc;
procedure IntegratePCStates;

implementation

uses ArrayDef, DSSGlobals, Utilities, MathUtil, Math, Fault, uComplex, YMatrix,
  PCElement, Spectrum, Vsource, Isource;

var
  ProgressCount: integer;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure EndOfTimeStepCleanup;
{
   Put stuff in this procedure that needs to happen at the end of the time step
   in main solution loops (see below)
}
begin
  writeln('nothing');
  //StorageClass.UpdateAll;
  //InvControlClass.UpdateAll;
end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure Show10PctProgress(i, N: integer);

begin
  if NoFormsAllowed then
    Exit;

  if ((i * 10) div N) > ProgressCount then
  begin
    Inc(ProgressCount);
    //ShowPctProgress(ProgressCount * 10);
  end;
end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveYearly: integer;
var
  N, Twopct: integer;
begin
  Result := 0;
  //ProgressCaption('Solving Year ' + IntToStr(ActiveCircuit.Solution.Year));
  ProgressCount := 0;
  //ShowPctProgress(ProgressCount);

  with ActiveCircuit, ActiveCircuit.Solution do
  begin
    try
      IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters and storage elements
      if not DIFilesAreOpen then
        EnergyMeterClass.OpenAllDIFiles;
      // Open Demand Interval Files, if desired   Creates DI_Totals
      Twopct := Max(NumberOfTimes div 50, 1);
      for N := 1 to NumberOfTimes do
        if not SolutionAbort then
          with Dynavars do
          begin
            Increment_time;
            DefaultHourMult := DefaultYearlyShapeObj.getmult(dblHour);
            if PriceCurveObj <> nil then
              PriceSignal := PriceCurveObj.GetPrice(dblHour);
            SolveSnap;
            MonitorClass.SampleAll;  // Make all monitors take a sample
            EnergyMeterClass.SampleAll; // Make all Energy Meters take a sample

            EndOfTimeStepCleanup;

            if (N mod Twopct) = 0 then
            writeln('no');
            //ShowPctProgress((N * 100) div NumberofTimes);
          end;
    finally
      //ProgressHide;
      MonitorClass.SaveAll;
      // EnergyMeterClass.CloseAllDIFiles;   // Save Demand interval Files    See DIFilesAreOpen Logic
    end;
  end;
end;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveDaily: integer;

{
  Solves following the daily load curve.
  Stepsize defaults to 1 hr and number of times = 24.
  Load is modified by yearly growth, time of day, and global load multiplier.
}

var
  N: integer;

begin
  Result := 0;

  with ActiveCircuit, ActiveCircuit.Solution do
  begin
    // t:=0.0;
    // MonitorClass.ResetAll;
    // EnergyMeterClass.ResetAll;
    try

      IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters
      if not DIFilesAreOpen then
        EnergyMeterClass.OpenAllDIFiles;   // Append Demand Interval Files, if desired

      for N := 1 to NumberOfTimes do
        if not SolutionAbort then
          with DynaVars do
          begin
            Increment_time;
            DefaultHourMult := DefaultDailyShapeObj.getmult(dblHour);
            if PriceCurveObj <> nil then
              PriceSignal := PriceCurveObj.GetPrice(dblHour);
            SolveSnap;
            MonitorClass.SampleAll;  // Make all monitors take a sample
            EnergyMeterClass.SampleAll; // Make all Energy Meters take a sample
            EndOfTimeStepCleanup;
          end;

    finally
      MonitorClass.SaveAll;
      EnergyMeterClass.CloseAllDIFiles;   // Save Demand interval Files
    end; {Try}
  end;  {WITH}
end;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolvePeakDay: integer;

{
 Solves peak day

    Takes the given load kW and assumes it represents the peak value.
    Load is modified by daily load curve and growth factor for the year.
    'h' defaults to 3600 (1 hr) but can be reset to anything.
    Differs from Daily mode in that the global load multiplier is ignored.
}

var
  N: integer;

begin
  Result := 0;
  with ActiveCircuit, ActiveCircuit.Solution do
  begin
    DynaVars.t := 0.0;
    // MonitorClass.ResetAll;
    // EnergyMeterClass.ResetAll;
    try
      DynaVars.intHour := 0;
      DynaVars.dblHour := 0.0;
      IntervalHrs := DynaVars.h / 3600.0;
      // needed for energy meters and storage devices
      if not DIFilesAreOpen then
        EnergyMeterClass.OpenAllDIFiles;   // Open Demand Interval Files, if desired

      for N := 1 to NumberOfTimes do
        if not SolutionAbort then
          with DynaVars do
          begin
            Increment_time;
            DefaultHourMult := DefaultDailyShapeObj.GetMult(dblHour);
            if PriceCurveObj <> nil then
              PriceSignal := PriceCurveObj.GetPrice(dblHour);
            SolveSnap;
            MonitorClass.SampleAll;  // Make all monitors take a sample
            EnergyMeterClass.SampleAll; // Make all Energy Meters take a sample

            EndOfTimeStepCleanup;

          end;
    finally
      MonitorClass.SaveAll;
      EnergyMeterClass.CloseAllDIFiles;   // Save Demand interval Files
    end;
  end;  {WITH}
end;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveDuty: integer;

var
  N, TwoPct: integer;

begin
  Result := 0;
  //ProgressCaption('Duty Cycle Solution');
  ProgressCount := 0;
  //ShowPctProgress(0);

  with ActiveCircuit, ActiveCircuit.Solution do
  begin
    //   t:=0.0;
    // MonitorClass.ResetAll;
    TwoPct := Max(1, NumberOfTimes div 50);
    try
      IntervalHrs := DynaVars.h / 3600.0;
      // needed for energy meters and storage devices
      for N := 1 to NumberOfTimes do
        if not SolutionAbort then
          with DynaVars do
          begin
            Increment_time;
            DefaultHourMult := DefaultDailyShapeObj.getmult(dblHour);
            // Assume pricesignal stays constant for dutycycle calcs
            SolveSnap;
            MonitorClass.SampleAll;  // Make all monitors take a sample

            EndOfTimeStepCleanup;


            if (N mod Twopct) = 0 then
            writeln('nothing');
            //ShowPctProgress((N * 100) div NumberofTimes);
          end;
    finally
      MonitorClass.SaveAll;
      //ProgressHide;
    end;
  end;
end;

function SolveGeneralTime: integer;

{
   For Rolling your own solution modes
}
var
  N: integer;

begin
  Result := 0;

  with ActiveCircuit, ActiveCircuit.Solution do
  begin
    IntervalHrs := DynaVars.h / 3600.0;
    // needed for energy meters and storage devices
    for N := 1 to NumberOfTimes do
      if not SolutionAbort then
        with DynaVars do
        begin
          {Compute basic multiplier from Default loadshape to use in generator dispatch, if any}
          DefaultHourMult := DefaultDailyShapeObj.getmult(dblHour);

          SolveSnap;
          MonitorClass.SampleAll;  // Make all monitors take a sample

          EndOfTimeStepCleanup;

          Increment_time;
        end;
  end;
end;




//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure IntegratePCStates;
 {Integrate states in all PC Elements.  At present, only PC Elements
  can have dynamic states}

var
  pcelem: TPCElement;

begin
  with ActiveCircuit do
  begin
    pcelem := PCelements.First;
    while pcelem <> nil do
    begin
      pcelem.IntegrateStates;
      pcelem := PCelements.Next;
    end;
  end;
end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveDynamic: integer;

var
  N: integer;

begin
  Result := 0;

  with ActiveCircuit, ActiveCircuit.Solution do
  begin
    try
      SolutionInitialized := True;
      // If we're in dynamics mode, no need to re-initialize.
      IntervalHrs := DynaVars.h / 3600.0;
      // needed for energy meters and storage devices
      for N := 1 to NumberOfTimes do
        if not SolutionAbort then
          with DynaVars do
          begin
            Increment_time;
            DefaultHourMult := DefaultDailyShapeObj.getmult(dblHour);
            // Assume price signal stays constant for dynamic calcs
            {Predictor}
            IterationFlag := 0;
            IntegratePCStates;
            SolveSnap;
            {Corrector}
            IterationFlag := 1;
            IntegratePCStates;
            SolveSnap;
            MonitorClass.SampleAll;  // Make all monitors take a sample

            EndOfTimeStepCleanup;

          end;
    finally
      MonitorClass.SaveAll;
    end;
  end;
end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveMonte1: integer;

var
  N: integer;

begin
  Result := 0;

  with ActiveCircuit, ActiveCircuit.Solution do
  begin
    try
      LoadMultiplier := 1.0;   // Always set with prop in case matrix must be rebuilt
      IntervalHrs := 1.0;     // needed for energy meters and storage devices
      DynaVars.intHour := 0;
      DynaVars.dblHour := 0.0;// Use hour to denote Case number
      DynaVars.t := 0.0;

      // MonitorClass.ResetAll;
      // EnergyMeterClass.ResetAll;

      //ProgressCaption('Monte Carlo Mode 1, ' + IntToStr(NumberofTimes) +
      //  ' Random Loads.');
      ProgressCount := 0;

      for N := 1 to NumberOfTimes do
        if not SolutionAbort then
        begin
          Inc(DynaVars.intHour);
          SolveSnap;
          MonitorClass.SampleAll;  // Make all monitors take a sample
          EnergyMeterClass.SampleAll;  // Make all meters take a sample
          Show10PctProgress(N, NumberOfTimes);
        end
        else
        begin
          ErrorNumber := SOLUTION_ABORT;
          CmdResult := ErrorNumber;
          GlobalResult := 'Solution Aborted';
          Break;
        end;
    finally
      MonitorClass.SaveAll;
      //ProgressHide;
    end;
  end;

end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveMonte2: integer;

  // Do a daily load solution for several Random days

var
  i, N, Ndaily: integer;

begin
  Result := 0;

  with ActiveCircuit, ActiveCircuit.solution do
  begin
    try
      DynaVars.t := 0.0;
      DynaVars.intHour := 0;
      DynaVars.dblHour := 0.0;
      // MonitorClass.ResetAll;
      // EnergyMeterClass.ResetAll;
      IntervalHrs := DynaVars.h / 3600.0;
      // needed for energy meters and storage devices
      Ndaily := Round(24.0 / IntervalHrs);

      if not DIFilesAreOpen then
        EnergyMeterClass.OpenAllDIFiles;   // Open Demand Interval Files, if desired

      //ProgressCaption('Monte Carlo Mode 2, ' + IntToStr(NumberofTimes) + ' Days.');
      ProgressCount := 0;

      for N := 1 to NumberOfTimes do

        if not SolutionAbort then
        begin       // Number of Days

          // Always set LoadMultiplier WITH prop in case matrix must be rebuilt
          case Randomtype of
            UNIFORM: LoadMultiplier := Random;  // number between 0 and 1
            GAUSSIAN: LoadMultiplier :=
                Gauss(DefaultDailyShapeObj.Mean, DefaultDailyShapeObj.StdDev);
          end;

          with DynaVars do
            for i := 1 to Ndaily do
            begin
              Increment_time;
              DefaultHourMult := DefaultDailyShapeObj.GetMult(dblHour);
              SolveSnap;

              MonitorClass.SampleAll;  // Make all monitors take a sample
              EnergyMeterClass.SampleAll;  // Make all meters take a sample

              EndOfTimeStepCleanup;

            end;

          Show10PctProgress(N, NumberOfTimes);

        end
        else
        begin
          ErrorNumber := SOLUTION_ABORT;
          CmdResult := ErrorNumber;
          GlobalResult := 'Solution Aborted.';
          Break;
        end;
    finally
      MonitorClass.SaveAll;
      EnergyMeterClass.CloseAllDIFiles;   // Save Demand interval Files
      //ProgressHide;
    end;
  end;
end;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveMonte3: integer;

  // Hold time fixed and just vary the global load multiplier

var
  N: integer;

begin
  Result := 0;

  with ActiveCircuit, ActiveCircuit.Solution do
  begin
    // Time must be set beFore entering this routine
    try
      // MonitorClass.ResetAll;
      // EnergyMeterClass.ResetAll;
      IntervalHrs := 1.0;  // just get per unit energy and multiply result as necessary

      if not DIFilesAreOpen then
        EnergyMeterClass.OpenAllDIFiles;   // Open Demand Interval Files, if desired

      //ProgressCaption('Monte Carlo Mode 3, ' + IntToStr(NumberofTimes) +
      //  ' Different Load Levels.');
      ProgressCount := 0;

      DefaultHourMult := DefaultDailyShapeObj.GetMult(DynaVars.dblHour);
      if PriceCurveObj <> nil then
        PriceSignal := PriceCurveObj.GetPrice(DynaVars.dblHour);

      for N := 1 to NumberOfTimes do
        if not SolutionAbort then
        begin

          // Always set LoadMultiplier WITH prop in case matrix must be rebuilt
          case Randomtype of
            UNIFORM: LoadMultiplier := Random;  // number between 0 and 1
            GAUSSIAN: LoadMultiplier :=
                Gauss(DefaultDailyShapeObj.Mean, DefaultDailyShapeObj.StdDev);
            LOGNORMAL: LoadMultiplier := QuasiLognormal(DefaultDailyShapeObj.Mean);
          end;

          SolveSnap;

          MonitorClass.SampleAll;  // Make all monitors take a sample
          EnergyMeterClass.SampleAll;  // Make all meters take a sample

          Show10PctProgress(N, NumberOfTimes);
        end
        else
        begin
          CmdResult := SOLUTION_ABORT;
          ErrorNumber := CmdResult;
          GlobalResult := 'Solution Aborted';
          Break;
        end;
    finally
      MonitorClass.SaveAll;
      EnergyMeterClass.CloseAllDIFiles;   // Save Demand interval Files
      //ProgressHide;
    end;
  end; {WITH}
end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveLD1: integer;

  // Do a Daily Simulation based on a load duration curve

var
  N, Ndaily, i: integer;

begin
  Result := 0;

  with ActiveCircuit, ActiveCircuit.Solution do
  begin
    try
      if LoadDurCurveObj = nil then
      begin
        Dosimplemsg(
          'Load Duration Curve Not Defined (Set LDCurve=... command). Cannot perForm solution.', 470);
        Exit;
      end;

      // Time must be set beFore entering this routine
      // MonitorClass.ResetAll;
      // EnergyMeterClass.ResetAll;

      NDaily := Round(24.0 / DynaVars.h * 3600.0);

      if not DIFilesAreOpen then
        EnergyMeterClass.OpenAllDIFiles;   // Open Demand Interval Files, if desired

      //ProgressCaption('Load-Duration Mode 1 Solution. ');

      // (set in Solve method) DefaultGrowthFactor :=  IntPower(DefaultGrowthRate, (Year-1));

      DynaVars.intHour := 0;
      with DynaVars do
        for i := 1 to Ndaily do
        begin

          // Set the time
          Increment_time;

          DefaultHourMult := DefaultDailyShapeObj.GetMult(dblHour);

          if not SolutionAbort then
          begin
            for N := 1 to LoadDurCurveObj.NumPoints do
            begin

              LoadMultiplier := LoadDurCurveObj.Mult(N);
              // Always set LoadMultiplier with prop in case matrix must be rebuilt
              // Adjust meter interval to interval on value of present Load-Duration Curve
              IntervalHrs := LoadDurCurveObj.PresentInterval;

              // Price curve must correspond to load-duration curve
              if PriceCurveObj <> nil then
                PriceSignal := PriceCurveObj.Price(N);

              SolveSnap;

              MonitorClass.SampleAll;     // Make all monitors take a sample
              EnergyMeterClass.SampleAll;  // Make all meters take a sample

              EndOfTimeStepCleanup;

            end;
            //ShowPctProgress((i * 100) div NDaily);
          end
          else
          begin
            CmdResult := SOLUTION_ABORT;
            ErrorNumber := CmdResult;
            GlobalResult := 'Solution Aborted';
            Break;
          end;

        end;
    finally
      MonitorClass.SaveAll;
      EnergyMeterClass.CloseAllDIFiles;   // Save Demand interval Files
      //ProgressHide;
    end;
  end; {WITH ActiveCircuit}

end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveLD2: integer;

  // Hold time fixed and just vary the global load multiplier according to the global
  // Load-Duration Curve

var
  N: integer;

begin
  Result := 0;

  with ActiveCircuit, ActiveCircuit.Solution do
  begin
    if LoadDurCurveObj = nil then
    begin
      Dosimplemsg('Load Duration Curve Not Defined (Set LDCurve=... command). Cannot perForm solution.', 471);
      Exit;
    end;

    // Time must be set beFore entering this routine


    // MonitorClass.ResetAll;
    // EnergyMeterClass.ResetAll;

    DefaultHourMult := DefaultDailyShapeObj.GetMult(DynaVars.dblHour);
    if not DIFilesAreOpen then
      EnergyMeterClass.OpenAllDIFiles;   // Open Demand Interval Files, if desired

    // (set in Solve Method) DefaultGrowthFactor :=  IntPower(DefaultGrowthRate, (Year-1));

    try
      if SolutionAbort then
      begin
        CmdResult := SOLUTION_ABORT;
        ErrorNumber := CmdResult;
        GlobalResult := 'Solution Aborted.';
        Exit;
      end;

      for N := 1 to LoadDurCurveObj.NumPoints do
      begin

        // Adjust meter interval to interval on value of present Load-Duration Curve
        LoadMultiplier := LoadDurCurveObj.Mult(N);
        // Always set LoadMultiplier WITH prop in case matrix must be rebuilt
        IntervalHrs := LoadDurCurveObj.PresentInterval;

        // Price curve must correspond to load-duration curve
        if PriceCurveObj <> nil then
          PriceSignal := PriceCurveObj.Price(N);

        SolveSnap;

        MonitorClass.SampleAll;  // Make all monitors take a sample
        EnergyMeterClass.SampleAll;  // Make all meters take a sample

        EndOfTimeStepCleanup;

      end;
    finally
      MonitorClass.SaveAll;
      EnergyMeterClass.CloseAllDIFiles;   // Save Demand interval Files
    end;
  end; {WITH ActiveCircuit}

end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure PickAFault;
// Enable one of the faults in the circuit.  Disable the rest
var
  NumFaults, i, Whichone: integer;
  FaultObj: TFaultObj;
begin
  NumFaults := ActiveCircuit.Faults.Listsize;
  Whichone := Trunc(Random * NumFaults) + 1;
  if Whichone > NumFaults then
    Whichone := NumFaults;

  for i := 1 to NumFaults do
  begin
    FaultObj := ActiveCircuit.Faults.Get(i);
    if i = Whichone then
    begin
      ActiveFaultObj := FaultObj; // in Fault Unit
      FaultObj.Enabled := True;
    end
    else
      FaultObj.Enabled := False;
  end;
end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveMonteFault: integer;

var
  N: integer;

begin
  Result := 0;

  with ActiveCircuit, ActiveCircuit.Solution do
  begin
    try
      LoadModel := ADMITTANCE;   // All Direct solution
      LoadMultiplier := 1.0;
      // Always set LoadMultiplier WITH prop in case matrix must be rebuilt
      DynaVars.intHour := 0;
      DynaVars.dblHour := 0.0; // Use hour to denote Case number
      DynaVars.t := 0.0;


      // MonitorClass.ResetAll;

      //ProgressCaption('Monte Carlo Fault Study: ' + IntToStr(NumberofTimes) +
      //  ' Different Faults.');
      ProgressCount := 0;

      SetGeneratorDispRef;

      for N := 1 to NumberOfTimes do
        if not SolutionAbort then
        begin
          Inc(DynaVars.intHour);
          PickAFault;  // Randomly enable one of the faults
          ActiveFaultObj.Randomize;  // Randomize the fault resistance
          SolveDirect;
          MonitorClass.SampleAll;  // Make all monitors take a sample

          Show10PctProgress(N, NumberOfTimes);
        end;
    finally
      MonitorClass.SaveAll;
      //ProgressHide;
    end;
  end;

end;

{--------------------------------------------------------------------------}
procedure AllocateAllSCParms;
var
  i: integer;
begin
  with ActiveCircuit do
  begin
    for i := 1 to NumBuses do
      Buses^[i].AllocateBusQuantities;
  end;
end;


{--------------------------------------------------------------------------}
procedure ComputeIsc;
{ Compute Isc at all buses for current values of Voc and Ysc }
var
  i: integer;
begin
  with ActiveCircuit do
  begin
    for i := 1 to NumBuses do
      with Buses^[i] do
      begin
        Ysc.MVMult(BusCurrent, VBus);
      end;
  end;
end;


{--------------------------------------------------------------------------}
procedure ComputeYsc(iB: integer);

{Compute YSC for I-th bus}
{Assume InjCurr is zeroed}

var
  i, j, ref1: integer;

begin
  writeln('null');
end;


{--------------------------------------------------------------------------}
procedure ComputeAllYsc;
var
  iB, j: integer;

begin

  with ActiveCircuit, ActiveCircuit.Solution do
  begin

    for j := 1 to NumNodes do
      Currents^[j] := cZERO;

    ProgressCount := 0;

    for iB := 1 to NumBuses do
    begin
      ComputeYsc(iB);  // Compute YSC for iB-th Bus
      if ((iB * 10) div NumBuses) > ProgressCount then
      begin
        Inc(ProgressCount);
        //ShowPctProgress(30 + ProgressCount * 5);
      end;
    end;
  end;
end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure DisableAllFaults;
begin
  with ActiveCircuit do
  begin
    ActiveFaultObj := Faults.First;
    while ActiveFaultObj <> nil do
    begin
      ActiveFaultObj.Enabled := False;
      ActiveFaultObj := Faults.Next;
    end;
  end;
end;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveFaultStudy: integer;
begin
  Result := 0;

  //ShowPctProgress(0);
  //ProgressCaption('Computing Open-Circuit Voltages');

  with ActiveCircuit.solution do
  begin
    LoadModel := ADMITTANCE;
    DisableAllFaults;
    SolveDirect;   // This gets the open circuit voltages and bus lists corrected

    AllocateAllSCParms;   // Reallocate bus quantities
    UpdateVBus;  // Put present solution Voc's in bus quantities
  end;

  //ProgressCaption('Computing Ysc Matrices for Each Bus');
  //ShowPctProgress(30);
  ComputeAllYsc;

  //ProgressCaption('Computing Short-circuit currents.');
  //ShowPctProgress(80);
  ComputeIsc;

  //ShowPctProgress(100);
  //ProgressCaption('Done.');
  //ProgressHide;
  // Now should have all we need to make a short circuit report

end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure AddFrequency(var FreqList: pDoublearray;
  var NumFreq, MaxFreq: integer; F: double);

{Add unique Frequency, F to list in ascending order, reallocating if necessary}

var
  i, j: integer;

begin

  {See if F is in List}

  for i := 1 to NumFreq do
  begin
    {Allow a little tolerance (0.1 hz) for the Frequency for round off error}
    if Abs(F - FreqList^[i]) < 0.1 then
      Exit; // Already in List, nothing to do
  end;

  {OK, it's not in list, so let's Add it}
  Inc(NumFreq);
  if NumFreq > MaxFreq then
  begin  // Let's make a little more room
    Inc(MaxFreq, 20);
    ReallocMem(FreqList, SizeOf(FreqList^[1]) * MaxFreq);
  end;

  {Let's add it in ascending order}
  for i := 1 to NumFreq - 1 do
  begin
    if F < FreqList^[i] then
    begin
      {Push down array and insert it}
      for j := NumFreq - 1 downto i do
        FreqList^[j + 1] := FreqList^[j];
      FreqList^[i] := F;
      Exit;  // We're done!
    end;
  end;

  {If we fall through, tack it on to the end}
  FreqList^[NumFreq] := F;

end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function GetSourceFrequency(pc: TPCElement): double;

var
  pVsrc: TVsourceObj;
  pIsrc: TIsourceObj;
begin

  if Comparetext(pc.DSSClassName, 'vsource') = 0 then
  begin
    pVsrc := pc as TVsourceObj;
    Result := pVsrc.srcFrequency;
  end
  else
  begin
    pIsrc := pc as TIsourceObj;
    Result := pIsrc.srcFrequency;
  end;

end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure CollectAllFrequencies(var FreqList: pDoubleArray; var NumFreq: integer);

var
  SpectrumInUse: pIntegerArray;
  p: TPCElement;
  MaxFreq, i, j: integer;
  pSpectrum: TSpectrumObj;
  f: double;

begin
  {Make a List of all frequencies in Use}

  {accumulate all unique Frequencies}
  MaxFreq := 20;    // Initial List size
  NumFreq := 0;
  Reallocmem(FreqList, Sizeof(FreqList^[1]) * MaxFreq);

  with ActiveCircuit do
  begin
    {Check Sources -- each could have a different base frequency}
    p := Sources.First;
    while p <> nil do
    begin
      if p.Enabled then
        if SpectrumClass.Find(p.Spectrum) <> nil then
        begin
          pSpectrum := SpectrumClass.GetActiveObj;
          f := GetSourceFrequency(p);
          for j := 1 to pSpectrum.NumHarm do
          begin
            AddFrequency(FreqList, NumFreq, MaxFreq,
              pSpectrum.HarmArray^[j] * f);
          end;
        end;
      p := Sources.Next;
    end;
  end;

  {Mark Spectra being used}
  {Check loads and generators - these are assumed to be at fundamental frequency}
  SpectrumInUse := AllocMem(SizeOf(SpectruminUse^[1]) * SpectrumClass.ElementCount);
  //Allocate and zero
  with ActiveCircuit do
  begin
    p := PCelements.First;
    while p <> nil do
    begin
      if p.Enabled then
        if SpectrumClass.Find(p.Spectrum) <> nil then
        begin
          SpectrumInUse^[SpectrumClass.Active] := 1;
        end;
      p := PCelements.Next;
    end;
  end; {With}

  {Add marked Spectra to list}
  for i := 1 to SpectrumClass.ElementCount do
  begin
    if SpectrumInUse^[i] = 1 then
    begin
      SpectrumClass.Active := i;
      pSpectrum := SpectrumClass.GetActiveObj;
      for j := 1 to pSpectrum.NumHarm do
      begin
        AddFrequency(FreqList, NumFreq, MaxFreq,
          pSpectrum.HarmArray^[j] * ActiveCircuit.Fundamental);
      end;
    end;
  end;

  ReallocMem(SpectrumInUse, 0);

end;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveHarmonic: integer;

var
  FrequencyList: pDoubleArray;
  i, NFreq: integer;

begin
  Result := 0;

  FrequencyList := nil;   // Set up for Reallocmem
  //ShowPctProgress(0);
  //ProgressCaption('Performing Harmonic Solution');

  with ActiveCircuit, ActiveCircuit.solution do
  begin
    try

      if Frequency <> Fundamental then
      begin     // Last solution was something other than fundamental
        Frequency := Fundamental;
        if not RetrieveSavedVoltages then
          Exit;  {Get Saved fundamental frequency solution}
      end;

      MonitorClass.SampleAll;   // Store the fundamental frequency in the monitors

      { Get the list of Harmonic Frequencies to solve at}
      if DoAllHarmonics then
        CollectAllFrequencies(FrequencyList, NFreq)   // Allocates FrequencyList
      else
      begin
        Reallocmem(FrequencyList, Sizeof(FrequencyList^[1]) * HarmonicListSize);
        NFreq := HarmonicListSize;
        for i := 1 to NFreq do
          FrequencyList^[i] := Fundamental * HarmonicList^[i];
      end;

      for i := 1 to NFreq do
      begin

        Frequency := FrequencyList^[i];
        if Abs(Harmonic - 1.0) > EPSILON then
        begin    // Skip fundamental
          //ProgressCaption('Solving at Frequency = ' + Format('%-g', [Frequency]));
          //ShowPctProgress(Round((100.0 * i) / Nfreq));
          SolveDirect;
          MonitorClass.SampleAll;
          // Storage devices are assumed to stay the same since there is no time variation in this mode
        end;

      end; {FOR}

      //ShowPctProgress(100);
      //ProgressCaption('Done.');
    finally
      //ProgressHide;
      MonitorClass.SaveAll;
      ReallocMem(FrequencyList, 0);
    end;
    // Now should have all we need to make a short circuit report

  end;

end;


end.
