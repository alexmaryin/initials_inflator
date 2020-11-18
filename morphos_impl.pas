unit morphos_impl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpsend, fpjson, strutils, fpjsonrtti,
  morher_interface;

const
  MORPHOS_URL = 'http://morphos.io/api/inflect-name?name=%s&_format=json';
  MORPHOS_ERROR = 'Сервис ответил ошибкой';

type

  { TMorphosResponse }

  TMorphosResponse = class(TPersistent)
  private
    fCases: TStrings;
    fGender: string;
    fName: string;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property name: string read fName write fName;
    property cases: TStrings read fCases write fCases;
    property gender: string read fGender write fGender;
  end;

  { TMorphosImpl }

  TMorphosImpl = class(TInterfacedObject, IInitialsMorpher)
  private
    HTTPSender: THTTPSend;
    JSONStreamer: TJSONDeStreamer;
    response: TMorphosResponse;
  public
    function GetInitials (Initials: string): CasesResponse;
    function GetWordsCase (Words: string): CasesResponse;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TMorphosResponse }

constructor TMorphosResponse.Create;
begin
  cases := TStringList.Create;
end;

destructor TMorphosResponse.Destroy;
begin
  cases.Free;
  inherited Destroy;
end;

{ TMorphosImpl }

function TMorphosImpl.GetInitials(Initials: string): CasesResponse;
var
  inf: TWordCase;
  i: integer = 0;
begin
  HTTPSender.Clear;
  if not HTTPSender.HTTPMethod('GET',Replacetext(Format(MORPHOS_URL, [Initials]), ' ', '+')) then
    raise EInOutError.Create(MORPHOS_ERROR);
  JSONStreamer.JSONToObject(GetJSON(HTTPSender.Document) as TJSONObject,response);
  for inf in TWordCase do begin
      Result[inf] := response.cases[i];
      inc(i);
  end;
end;

function TMorphosImpl.GetWordsCase(Words: string): CasesResponse;
begin
  Result := GetInitials(Words);
end;

constructor TMorphosImpl.Create;
begin
  HTTPSender := THTTPSend.Create;
  JSONStreamer := TJSONDeStreamer.Create(nil);
  response := TMorphosResponse.Create;
end;

destructor TMorphosImpl.Destroy;
begin
  FreeAndNil(HTTPSender);
  FreeAndNil(JSONStreamer);
  FreeAndNil(response);
  inherited Destroy;
end;

end.


