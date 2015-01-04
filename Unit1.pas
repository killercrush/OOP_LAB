unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, FigureClass, ComCtrls, ToolWin, Menus, ImgList;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    N1: TMenuItem;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    N2: TMenuItem;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Label1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure N1Click(Sender: TObject);
    procedure N2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  FigureList: TList;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  FreeAndNil(FigureList);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FigureList := TList.Create;
end;

procedure TForm1.Label1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Point: TDot;
  Circle: TCircle;
  Square: TSquare;
  ListItem, NextItem: PListItem;
begin
  if not Assigned(FigureList) then
    FigureList := TList.Create;
  if Button = mbLeft then
  begin
    if ToolButton1.Down then
    begin

    end;
    if ToolButton2.Down then
    begin
      Point := TDot.Create(X, Y, $0, Label1.Canvas);
      FigureList.AddItem(Point);
      // Point.Draw;
    end;
    if ToolButton3.Down then
    begin
      Circle := TCircle.Create(X, Y, $0, Label1.Canvas, 50);
      FigureList.AddItem(Circle);
      // Circle.Draw;
    end;
    if ToolButton4.Down then
    begin
      Square := TSquare.Create(X, Y, $0, Label1.Canvas, 50);
      FigureList.AddItem(Square);
      // Square.Draw;
    end;
    if FigureList.FirstItem = nil then
      exit;
    ListItem := FigureList.FirstItem;
    repeat
      NextItem := ListItem.NextItem;
      ListItem.Item.Selected := ListItem.Item.PointInFugure(X, Y);
      ListItem := NextItem;
    until ListItem = nil;
  end;
end;

procedure TForm1.N1Click(Sender: TObject);
var
  CurPos: TPoint;
  ListItem, NextItem: PListItem;
begin
  GetCursorPos(CurPos);
  if FigureList.FirstItem = nil then
    exit;
  ListItem := FigureList.FirstItem;
  repeat
    if ListItem.Item.Selected then
      ListItem.Item.Shift(CurPos.X - Form1.Left, CurPos.Y - Form1.Top);
    ListItem := ListItem.NextItem;
  until ListItem = nil;
end;

procedure TForm1.N2Click(Sender: TObject);
var
  CurPos: TPoint;
begin
  GetCursorPos(CurPos);
  ShowMessage(IntToStr(CurPos.X) + ' ' + IntToStr(CurPos.Y));
end;

end.