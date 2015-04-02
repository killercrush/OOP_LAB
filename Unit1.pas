unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, FigureClass, ComCtrls, ToolWin, Menus, ImgList;

type
  TForm1 = class(TForm)
    PopupMenu1: TPopupMenu;
    N1: TMenuItem;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    Panel2: TPanel;
    edInfo: TEdit;
    edSize: TEdit;
    bColor: TButton;
    ColorDialog1: TColorDialog;
    Label1: TLabel;
    Label2: TLabel;
    edAngle: TEdit;
    Label3: TLabel;
    bDelete: TButton;
    ToolButton5: TToolButton;
    bOKSize: TButton;
    bOKAngle: TButton;
    bColorOk: TButton;
    Label4: TLabel;
    procedure Form1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure N1Click(Sender: TObject);
    procedure bColorClick(Sender: TObject);
    procedure bDeleteClick(Sender: TObject);
    procedure edSizeExit(Sender: TObject);
    procedure bOKSizeClick(Sender: TObject);
    procedure bOKAngleClick(Sender: TObject);
    procedure bColorOkClick(Sender: TObject);
    procedure edAngleExit(Sender: TObject);
    procedure RefreshInfo(Figure: TDot);
    procedure CanvasChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  FigureList: TList;
  ClickPos: TPoint;

implementation

{$R *.dfm}

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(FigureList);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FigureList := TList.Create;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
if FigureList.IsEmpty then
  exit;
if Key = VK_RIGHT then
FigureList.CurrentItem.Item.Selected := false;
if FigureList.CurrentItem.NextItem = nil then
  FigureList.CurrentItem := FigureList.GetFirstItem
else
  FigureList.CurrentItem := FigureList.CurrentItem.NextItem;
FigureList.CurrentItem.Item.Selected := true;
RefreshInfo(FigureList.CurrentItem.Item);
end;

procedure TForm1.CanvasChange(Sender: TObject);
var
  ListItem: PListItem;
begin
  if not Assigned(FigureList) then
    FigureList := TList.Create;
  if FigureList.IsEmpty then
    exit;
  FigureList.Reset;
  ListItem := FigureList.CurrentItem;
  repeat
    ListItem.Item.Draw;
    ListItem.Item.DrawSelection;
    if ListItem.Item.Selected then
      FigureList.CurrentItem := ListItem;
    ListItem := ListItem.NextItem;
  until ListItem = nil;
end;

procedure TForm1.bColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then
    Label1.Color := ColorDialog1.Color;
  CanvasChange(self);
end;

procedure TForm1.bColorOkClick(Sender: TObject);
begin
  if not FigureList.IsEmpty then
    FigureList.CurrentItem.Item.Color := Label1.Color;
   CanvasChange(self);
end;

procedure TForm1.bDeleteClick(Sender: TObject);
begin
  if not FigureList.IsEmpty then
    FigureList.DeleteItem(FigureList.CurrentItem);
 CanvasChange(self);
  FigureList.Reset;
  edInfo.Clear;
end;

procedure TForm1.bOKAngleClick(Sender: TObject);
begin
  if not FigureList.IsEmpty then
    FigureList.CurrentItem.Item.Angle := StrToInt(edAngle.Text);
   CanvasChange(self);
end;

procedure TForm1.bOKSizeClick(Sender: TObject);
begin
  if not FigureList.IsEmpty then
    FigureList.CurrentItem.Item.Size := StrToInt(edSize.Text);
   CanvasChange(self);
end;

procedure TForm1.edAngleExit(Sender: TObject);
begin
  if edAngle.Text = '' then
    edAngle.Text := '0';
end;

procedure TForm1.edSizeExit(Sender: TObject);
begin
  if edSize.Text = '' then
    edSize.Text := '0';
end;

procedure TForm1.RefreshInfo(Figure: TDot);
begin
  edSize.Text := IntToStr(Figure.Size);
  edAngle.Text := IntToStr(Figure.Angle);
  Label1.Color := Figure.Color;
  edInfo.Text := Figure.Info;
end;

procedure TForm1.Form1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Point: TDot;
  Circle: TCircle;
  Square: TSquare;
  Star: TStar;
  ListItem: PListItem;
  FigureSelected: boolean;
begin
  ClickPos.X := X;
  ClickPos.Y := Y;
  if not Assigned(FigureList) then
    FigureList := TList.Create;
  if Button = mbLeft then
  begin
    if ToolButton1.Down then
    begin
      if FigureList.IsEmpty then
        exit;
      FigureList.Reset;
      ListItem := FigureList.CurrentItem;
      FigureSelected := false;
      repeat
        if not FigureSelected then
        begin
          ListItem.Item.Selected := ListItem.Item.PointInFugure(X, Y);
          FigureList.CurrentItem := ListItem;
          RefreshInfo(ListItem.Item);
          FigureSelected := ListItem.Item.Selected;
        end
        else
          ListItem.Item.Selected := false;
        ListItem := ListItem.NextItem;
      until ListItem = nil;
    end;
    if ToolButton2.Down then
    begin
      if not FigureList.IsEmpty then
        FigureList.CurrentItem.Item.Selected := false;
      Point := TDot.Create(X, Y, Label1.Color, Canvas);
      FigureList.AddItem(Point);
      RefreshInfo(Point);
    end;
    if ToolButton3.Down then
    begin
      if not FigureList.IsEmpty then
        FigureList.CurrentItem.Item.Selected := false;
      Circle := TCircle.Create(X, Y, Label1.Color, Canvas,
        StrToInt(edSize.Text));
      FigureList.AddItem(Circle);
      RefreshInfo(Circle);
    end;
    if ToolButton4.Down then
    begin
      if not FigureList.IsEmpty then
        FigureList.CurrentItem.Item.Selected := false;
      Square := TSquare.Create(X, Y, Label1.Color, Canvas,
        StrToInt(edSize.Text), StrToInt(edAngle.Text));
      FigureList.AddItem(Square);
      RefreshInfo(Square);
    end;
    if ToolButton5.Down then
    begin
      if not FigureList.IsEmpty then
        FigureList.CurrentItem.Item.Selected := false;
      Star := TStar.Create(X, Y, Label1.Color, Canvas, StrToInt(edSize.Text),
        StrToInt(edAngle.Text));
      FigureList.AddItem(Star);
      RefreshInfo(Star);
    end;
   CanvasChange(self);
  end;
end;

procedure TForm1.N1Click(Sender: TObject);
begin
  if not FigureList.IsEmpty then
    FigureList.CurrentItem.Item.Shift(ClickPos.X, ClickPos.Y);
  Form1.CanvasChange(self);
end;

end.
