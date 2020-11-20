unit restjson;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpsend, fpjson, fpjsonrtti;

resourcestring
  RESTOUT_ERROR = 'REST API service does not return any result';

{ Дальше идет объявление класса-пустышки только из-за того, что Lazarus
  не хочет воспринимать простое объявление дженерик-функции как верное,
  и не хочет дальше парсить код, автозаполнять и подсказывать... }

type
    TDummy = class
      end;

generic function JSONfromRestUri<T>(Uri: string): T;

implementation

generic function JSONfromRestUri<T>(Uri: string): T;
var
  HTTPSender: THTTPSend;
  JSONStreamer: TJSONDeStreamer;
  Json: TJSONObject;
begin
  HTTPSender := THTTPSend.Create;
  JSONStreamer := TJSONDeStreamer.Create(nil);
  HTTPSender.Clear;
  Result := T.Create;
  if not HTTPSender.HTTPMethod('GET', Uri) then raise EInOutError.Create(RESTOUT_ERROR);
  JSON := GetJSON(HTTPSender.Document) as TJSONObject;
  JSONStreamer.JSONToObject(JSON, Result);
  FreeAndNil(JSON);
  FreeAndNil(JSONStreamer);
  FreeAndNil(HTTPSender);
end;

end.
