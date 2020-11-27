unit dadata_impl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  morher_interface;

const
  DADATA_TOKEN = '';
  DADATA_XSECRET = '';

type

  { TDadataImpl }

  TDadataImpl = class(TInterfacedObject, IInitialsMorpher)
  public
    function GetInitials (Initials: string): CasesResponse;
   function GetWordsCase (Words: string): CasesResponse;
   function GetGenderAndInitials(Initials: string; var Gender: TGender): CasesResponse;
  end;

implementation

{ TDadataImpl }

function TDadataImpl.GetInitials(Initials: string): CasesResponse;
begin
   raise Exception.Create('No implemented yet!')
end;

function TDadataImpl.GetWordsCase(Words: string): CasesResponse;
begin
   raise Exception.Create('No implemented yet!');
end;

function TDadataImpl.GetGenderAndInitials(Initials: string; var Gender: TGender
  ): CasesResponse;
begin
  raise Exception.Create('No implemented yet!');
end;

end.

