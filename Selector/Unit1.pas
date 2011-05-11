unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uSelector, StdCtrls;

type
  TForm1 = class(TForm)
    btn1: TButton;
    btn2: TButton;
    btn3: TButton;
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
  private
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
  b := TSelector.Create;
//  b.Parent := Self;
  b.Show;
end;

procedure TForm1.btn2Click(Sender: TObject);
begin
  b.SetPosition(Point(50,100),400,200);
//  b.Parent := Self;
end;

procedure TForm1.btn3Click(Sender: TObject);
var
  r: TRect;
begin
  r := b.GetClient;
  r.TopLeft := ScreenToClient(r.TopLeft);
  r.BottomRight := ScreenToClient(r.BottomRight);
  ShowMessage('l:'+IntToStr(r.Left)+' r:'+inttostr(r.Right)+' t:'+IntToStr(r.Top)+' b:'+IntToStr(r.Bottom));
end;

end.
