unit morph_fabric;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, Generics.Defaults,
  morher_interface, pymorphy_impl, dadata_impl, morphos_impl;

type

  MorphProvider = (PYMORPHY, DADATA, MORPHOS);

  { TCachedValue }

  TCachedValue = record
    cases: CasesResponse;
    gender: TGender;
    lastUse: TDateTime;
  end;

  { TMorphFabric }

  TMorphFabric = class(TInterfacedObject, IInitialsMorpher)
  private
    Morpher: IInitialsMorpher;
    Cache: specialize THashMap<string, TCachedValue>;
    CacheAllowed: boolean;
    CachedValue: CasesResponse;
    CachedGender: TGender;
    function IsCached(Initials: string): boolean;
    procedure SavetoCache(cases: CasesResponse; gender: TGender);
  public
    function GetInitials (Initials: string): CasesResponse;
    function GetWordsCase (Words: string): CasesResponse;
    function GetGenderAndInitials(Initials: string; var Gender: TGender): CasesResponse;
    constructor Create(provider: MorphProvider; withCache: boolean = true);
    destructor Destroy; override;
  end;

implementation

{ TMorhFabric }

function TMorphFabric.IsCached(Initials: string): boolean;
begin
  Result := false;
end;

procedure TMorphFabric.SavetoCache(cases: CasesResponse; gender: TGender);
begin
  //
end;

function TMorphFabric.GetInitials(Initials: string): CasesResponse;
var
  MokeGender: TGender = UnrecognizedGender;
begin
  Result := GetGenderAndInitials(Initials, MokeGender);
end;

function TMorphFabric.GetWordsCase(Words: string): CasesResponse;
begin
  //
end;

function TMorphFabric.GetGenderAndInitials(Initials: string; var Gender: TGender
  ): CasesResponse;
begin
   if CacheAllowed and IsCached(Initials) then
     begin
       Result := CachedValue;
       Gender := CachedGender;
     end
   else
     begin
       Result := Morpher.GetGenderAndInitials(Initials, Gender);
       SavetoCache(Result, Gender);
     end;
end;

constructor TMorphFabric.Create(provider: MorphProvider; withCache: boolean);
begin
  case provider of
         PYMORPHY: Morpher := TPymorphyImpl.Create;
         DADATA: Morpher := TDadataImpl.Create;
         MORPHOS: Morpher := TMorphosImpl.Create;
    end;
  CacheAllowed := withCache;
  if CacheAllowed then begin
    // read from file to Cache
  end;
end;

destructor TMorphFabric.Destroy;
begin
  // write back Cache to file
  FreeAndNil(Cache);
  inherited Destroy;
end;


end.

