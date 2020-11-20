unit morph_fabric;

{$mode objfpc}{$H+}{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Generics.Collections, Generics.Defaults, fpjson, typinfo, lazUTF8,
  morher_interface, pymorphy_impl, dadata_impl, morphos_impl;

const
  INITIALS_CACHE_DIR = 'cache';
  INITIALS_CACHE_FILE = INITIALS_CACHE_DIR+PathDelim+'initials_cached.json';
type

  MorphProvider = (PYMORPHY, DADATA, MORPHOS);

  { TCachedValue }

  TCachedValue = record
    cases: CasesResponse;
    gender: TGender;
    lastUse: TDateTime;
    class function Create(_cases: CasesResponse; _gender: TGender;
          _lastuse: TDateTime): TCachedValue; static;
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
    procedure LoadCacheFromFile;
    procedure SaveCacheToFile;
    procedure SavetoCache(key: string; cases: CasesResponse; gender: TGender);
  public
    function GetInitials (Initials: string): CasesResponse;
    function GetWordsCase (Words: string): CasesResponse;
    function GetGenderAndInitials(Initials: string; var Gender: TGender): CasesResponse;
    constructor Create(provider: MorphProvider; withCache: boolean = true);
    destructor Destroy; override;
  end;

implementation

{ TCachedValue }

class function TCachedValue.Create(_cases: CasesResponse; _gender: TGender;
  _lastuse: TDateTime): TCachedValue;
begin
  Result.Cases := _cases;
  Result.Gender := _gender;
  Result.LastUSe := _lastuse;
end;

{ TMorhFabric }

function TMorphFabric.IsCached(Initials: string): boolean;
var
  value: TCachedValue;
begin
  Result := false;
  if Cache.TryGetValue(Initials,value) then
    begin
      CachedValue := value.cases;
      CachedGender := value.gender;
      value.lastUse:= Now();
      Cache.AddOrSetValue(Initials,value);
      Result := true;
    end;
end;

procedure TMorphFabric.LoadCacheFromFile;
var
  cached: specialize TPair<string, TCachedValue>;
  jarr: TJSONArray;
  i, j: integer;
  item: TJSONObject;
  jData: TJSONData;
  Cachefile: TStringList;
begin
  Cache := specialize THashMap<string, TCachedValue>.Create;
  Cachefile := TStringList.Create;
  if FileExists(INITIALS_CACHE_FILE) then Cachefile.LoadFromFile(INITIALS_CACHE_FILE);
  if Cachefile.Count > 0 then begin
    jData := GetJSON(Cachefile.Text);
    for i := 0 to jData.Count - 1 do begin
      item :=  TJSONObject(jData.Items[i]);
      cached.Key := item.Names[0];
      jArr := item.Arrays[cached.Key];
      for j := 0 to Ord(High(TWordCase)) do
        cached.Value.cases[TWordCase(j)] := jArr.Items[j].AsString;
      cached.Value.gender := TGender(GetEnumValue(TypeInfo(TGender),
        jArr.Items[6].AsString));
      cached.Value.lastUse := StrToDate(jArr.Items[7].AsString, 'd/m/y', '.');
      Cache.Add(cached);
    end;
    FreeAndNil(jData);
  end;
  CacheFile.Free;
end;

procedure TMorphFabric.SaveCacheToFile;
var
  str: string;
  jsonfile: TStringList;
  cached: specialize TPair<string, TCachedValue>;
  jarr, Json: TJSONArray;
  Item: TJSONObject;
begin
  Json := TJSONArray.Create;
  for cached in Cache do
    begin
      jarr := TJSONArray.Create;
      for str in cached.Value.cases do
        jarr.Add(str);
      jarr.Add(GetEnumName(TypeInfo(TGender), Ord(cached.Value.gender)));
      jarr.Add(FormatDateTime('DD.MM.YYYY', cached.Value.lastUse));
      Item := CreateJSONObject([cached.Key, jarr]);
      Json.Add(Item);
    end;
  jsonfile := TStringList.Create;
  jsonfile.Text := Json.FormatJSON();
  if not DirectoryExists(INITIALS_CACHE_DIR) then MkDir(INITIALS_CACHE_DIR);
  jsonfile.SaveToFile(INITIALS_CACHE_FILE);
  FreeAndNil(jsonfile);
  FreeAndNil(Json);
  FreeAndNil(Cache);
end;

procedure TMorphFabric.SavetoCache(key: string; cases: CasesResponse; gender: TGender);
begin
  if not Cache.ContainsKey(key) then
    Cache.Add(key,TCachedValue.Create(cases, gender, Now()))
  else
    raise Exception.Create('Key already cached');
end;

function TMorphFabric.GetInitials(Initials: string): CasesResponse;
var
  MokeGender: TGender = UnrecognizedGender;
begin
  Result := GetGenderAndInitials(Initials, MokeGender);
end;

function TMorphFabric.GetWordsCase(Words: string): CasesResponse;
var
  inf: TWordCase;
begin
  Result := GetInitials(Words);
  for inf in TWordCase do
    Result[inf] := UTF8LowerString(Result[inf]);
end;

function TMorphFabric.GetGenderAndInitials(Initials: string; var Gender: TGender): CasesResponse;
begin
   if CacheAllowed and IsCached(Initials) then begin
       Result := CachedValue;
       Gender := CachedGender;
     end
   else begin
       Result := Morpher.GetGenderAndInitials(Initials, Gender);
       if CacheAllowed then SavetoCache(Initials, Result, Gender);
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
  if CacheAllowed then LoadCacheFromFile;
end;

destructor TMorphFabric.Destroy;
begin
  if CacheAllowed then SaveCacheToFile;
  inherited Destroy;
end;

end.

