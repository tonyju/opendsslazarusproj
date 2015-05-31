unit OHLineConstants;

{$mode delphi}

interface

uses
  Classes, SysUtils,Arraydef, Ucmatrix, Ucomplex, LineUnits, LineConstants;

TYPE

TOHLineConstants = class(TLineConstants)
  private

  protected

  public
     Constructor Create(NumConductors:Integer);
     Destructor Destroy;  Override;
end;

implementation

constructor TOHLineConstants.Create( NumConductors: Integer);
begin
  inherited Create (NumConductors);
end;

destructor TOHLineConstants.Destroy;
begin
  inherited;
end;

initialization

end.
