unit morphos_impl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils, restjson, morher_interface, LazUtf8;

const
  MORPHOS_URL = 'http://morphos.io/api/inflect-name?name=%s&_format=json';

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
  public
    function GetInitials (Initials: string): CasesResponse;
    function GetWordsCase (Words: string): CasesResponse;
    function GetGenderAndInitials(Initials: string; var Gender: TGender): CasesResponse;
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
  MokeGender: TGender = UnrecognizedGender;
begin
  Result := GetGenderAndInitials(Initials, MokeGender);
end;

function TMorphosImpl.GetWordsCase(Words: string): CasesResponse;
var
  inf: TWordCase;
begin
  Result := GetInitials(Words);
  for inf in TWordCase do
    Result[inf] := UTF8LowerString(Result[inf]);
end;

function TMorphosImpl.GetGenderAndInitials(Initials: string; var Gender: TGender
  ): CasesResponse;
var
  inf: TWordCase;
  i: integer = 0;
  rest: specialize TRestApi<TMorphosResponse>;
  response: TMorphosResponse;
begin
  rest := specialize TRestApi<TMorphosResponse>.Create;
  response := rest.JSONfromRestUri(Replacetext(Format(MORPHOS_URL, [Initials]), ' ', '+'));
  for inf in TWordCase do begin
      Result[inf] := response.cases[i];
      inc(i);
  end;
  case response.gender of
       'm': Gender := Male;
       'f': Gender := Female;
       else Gender := UnrecognizedGender;
  end;
  response.Free;
  rest.Free;
end;

end.


