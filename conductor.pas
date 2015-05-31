unit Conductor;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

TYPE

  TConductor = class(TObject)
    private
      TCCName:String;
      AmbientTemp:Double;
      Accum_Isqt:Double; // Accumulated I2t
      Procedure Set_Ambient(Value:Double);
      Procedure Set_TCCname(const Value:String);

    public
      Closed:Boolean;    // change this variable to indicate open or closed switch
      FuseBlown:Boolean;
      Procedure CalcIsqt(CurrentMag:Double);  // Computes whether conductor has burned down
      Procedure ResetIsqt;  // restore the conductor and reset the i2t calcs

      constructor Create;
      destructor Destroy; override;

      Property Ambient:Double write Set_Ambient;
      Property TccCurve:String read TCCname write Set_TCCname;


  end;

  pTConductorArray = ^TConductorArray;
  TConductorArray = Array[1..1] of Tconductor;

implementation

USES Sysutils, DSSGlobals;

constructor TConductor.Create;
BEGIN
     Inherited Create;
     Closed := True;
     FuseBlown := False;
     Accum_Isqt := 0.0;
     TCCName := '';
END;

Destructor TConductor.Destroy;
BEGIN
     Inherited Destroy;
END;

Procedure TConductor.Set_Ambient(Value:Double);
BEGIN
    AmbientTemp := Value;
END;

Procedure TConductor.Set_TCCname(const Value:String);
BEGIN
    TCCname := lowercase(value);
END;


Procedure TConductor.CalcIsqt(CurrentMag:Double);  // Computes whether conductor has burned down
BEGIN
  DoSimpleMsg('Need to implement Tconductor.CalcIsqrt', 770);
END;

Procedure TConductor.ResetIsqt;  // restore the conductor and reset the i2t calcs
BEGIN
  DoSimpleMsg('Need to implement Tconductor.ResetIsqt', 771);
END;


end.
