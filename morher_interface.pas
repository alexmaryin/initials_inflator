unit morher_interface;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TWordCase = (Nominative, Gentitive, Dative, Accusative, Instrumental, Prepositional);
  TGender = (Male, Female, Unrecognized);

  CasesResponse = array[TWordCase] of string;

  { IInitialsMorpher }

  IInitialsMorpher = interface
    ['{2D4408CF-4475-4D4F-B1AF-4FA030F641BD}']
    // Returns initials in needed case in simple string
    function GetInitials(Initials: string): CasesResponse;

    // Returns words combination in needed case in simple string
    function GetWordsCase(Words: string): CasesResponse;

    // Returns initials in needed case in simple string with gender in var-parameter
    function GetGenderAndInitials(Initials: string; var Gender: TGender): CaseResponse;
  end;

implementation

end.

