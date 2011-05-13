unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uSelector, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    btn1: TButton;
    lbl1: TLabel;
    procedure btn1Click(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);

  private
  protected
                                                           
    { Private declarations }

  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  b: TSelector;
implementation

{$R *.dfm}


procedure TForm1.btn1Click(Sender: TObject);
begin
  b := TSelector.Create(Self);
  b.Show;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  lbl1.Caption := IntToStr(x);
  if Assigned(b) then
  Caption := IntToStr(b.GetCenterRect.Left) + ':' + IntToStr(Left);
end;

end.
