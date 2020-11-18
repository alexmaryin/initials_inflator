unit pymorphy_impl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpsend, fpjson, strutils, lazUTF8,
  morher_interface;

const
  PYMORPHY_URL = 'http://pyphrasy.herokuapp.com/inflect?phrase=%s&' +
    'cases=nomn&cases=gent&cases=datv&cases=accs&cases=ablt&cases=loct';
  PYMORPHY_ERROR = 'Сайт pyphrasy.herokuapp.com недоступен';
  CasesArr: array [TWordCase] of string =
    ('nomn', 'gent', 'datv', 'accs', 'ablt', 'loct');

type

  { TPymorphyImpl }

  TPymorphyImpl = class(TInterfacedObject, IInitialsMorpher)
  private
    HTTPSender: THTTPSend;
    jData: TJSONData;
    function GetWord(word: string): CasesResponse;
  public
    function GetInitials(Initials: string): CasesResponse;
    function GetWordsCase(Words: string): CasesResponse;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TPymorphyImpl }

function TPymorphyImpl.GetWord(word: string): CasesResponse;
var
  inf: TWordCase;
begin
  HTTPSender.Clear;
  if not HTTPSender.HTTPMethod('GET', Replacetext(Format(PYMORPHY_URL, [word]),
    ' ', '%20')) then
    raise EInOutError.Create(PYMORPHY_ERROR);
  jData := GetJSON(HTTPSender.Document);
  for inf in TWordCase do
    Result[inf] := jData.FindPath(CasesArr[inf]).AsString;
end;

function TPymorphyImpl.GetInitials(Initials: string): CasesResponse;
var
  w: string;
  inf: TWordCase;
  warr: Casesresponse;

  function CapitalizeFirst(Name: string): string;
  begin
    Result := UTF8UpperCase(UTF8Copy(Name, 1, 1)) +
      UTF8RightStr(Name, UTF8Length(Name) - 1);
  end;

begin
  for w in SplitString(Initials, ' ') do
  begin
    warr := GetWord(w);
    for inf in TWordCase do
      Result[inf] += CapitalizeFirst(warr[inf]) + ' ';
  end;
  for inf in TWordCase do
    Result[inf] := TrimRight(Result[inf]);
end;

function TPymorphyImpl.GetWordsCase(Words: string): CasesResponse;
var
  w: string;
  inf: TWordCase;
  warr: Casesresponse;
begin
  for w in SplitString(Words, ' ') do
  begin
    warr := GetWord(w);
    for inf in TWordCase do
      Result[inf] += warr[inf] + ' ';
  end;
  for inf in TWordCase do
    Result[inf] := TrimRight(Result[inf]);
end;

constructor TPymorphyImpl.Create;
begin
  HTTPSender := THTTPSend.Create;
end;

destructor TPymorphyImpl.Destroy;
begin
  FreeAndNil(HTTPSender);
  FreeAndNil(jData);
  inherited Destroy;
end;

end.
