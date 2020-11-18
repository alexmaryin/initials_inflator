unit testform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  morher_interface, morph_fabric;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    CheckInitial: TCheckBox;
    TestEdit: TEdit;
    Label1: TLabel;
    ResultMemo: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    Morpher: IInitialsMorpher;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  response: CasesResponse;
begin
  Morpher := TMorphFabric.GetActual(MORPHOS);
  ResultMemo.Clear;
  try
    if CheckInitial.Checked then
      response := Morpher.GetInitials(TestEdit.Text)
    else
      response := Morpher.GetWordsCase(TestEdit.Text);
    Resultmemo.Lines.AddStrings(response);
  except
    on E: Exception do
       Resultmemo.Lines.Add(E.ClassName + ': '+ E.Message);
  end;
end;

end.

