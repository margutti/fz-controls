{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit FZControlsPkg;

interface

uses
  FZBase, FZCommon, FZDB, FZRegister, udropdown, udbdropdown, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('FZRegister', @FZRegister.Register);
end;

initialization
  RegisterPackage('FZControlsPkg', @Register);
end.
