unit morph_fabric;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  morher_interface, pymorphy_impl, dadata_impl, morphos_impl;

type

  MorphProvider = (PYMORPHY, DADATA, MORPHOS);

  { TMorphFabric }

  TMorphFabric = class
  public
    class function GetActual(provider: MorphProvider): IInitialsMorpher;
  end;

implementation

{ TMorhFabric }

class function TMorphFabric.GetActual(provider: MorphProvider
  ): IInitialsMorpher;
begin
  case provider of
         PYMORPHY: Result := TPymorphyImpl.Create;
         DADATA: Result := TDadataImpl.Create;
         MORPHOS: Result := TMorphosImpl.Create;
    end;
end;

end.

