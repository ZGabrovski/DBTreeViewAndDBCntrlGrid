{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit dbcontrols;

{$warn 5023 off : no warning about unused units}
interface

uses
  DBTreeView, dbcntrlgrid, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('DBTreeView', @DBTreeView.Register);
  RegisterUnit('dbcntrlgrid', @dbcntrlgrid.Register);
end;

initialization
  RegisterPackage('dbcontrols', @Register);
end.
