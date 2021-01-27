
{
 /***************************************************************************
                               DBControlGrid.pas
                               -----------
                     An interface to DB aware Controls
                     Initial Revision : Sun Mar 8 2015


 ***************************************************************************/

 Unashameably hacked from DBGrid.Pas (Copyright (C) 2003  Jesus Reyes Aguilar.)
 by Tony Whyman (tony@mwasoftware.co.uk) .Additional source code is
 Copyright (c) McCallum Whyman Associates Ltd (trading as MWA Software) 2015.

 This unit defines TDBCntrlGrid: a lookalike rather than a clone for the Delphi
 TDBCrtlGrid. TDBCntrlGrid is a single column grid that replicates a TWinControl
 - typically a TPanel or a TFrame in each row. Each row corresponding to the rows
 of the linked DataSource. Any data aware control on the replicated (e.g.) TPanel
 will then appear to have the appropriate value for the row.

 The replicated control is not part of this control but must be added by the
 programmer at design time, and linked to the "DrawPanel" property.

 Rows can be edited, inserted (append only) or deleted.

 Distributed and licensed under the Library GNU General Public License
 see https://www.gnu.org/licenses/lgpl.html with the following modification:

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent modules,
and to copy and distribute the resulting executable under terms of your choice,
provided that you also meet, for each linked independent module, the terms
and conditions of the license of that module. An independent module is a
module which is not derived from or based on this library. If you modify this
library, you may extend this exception to your version of the library, but
you are not obligated to do so. If you do not wish to do so, delete this
exception statement from your version.

With a small bugfixes and change the component name to TBCntrlGrid from Zdravko Gabrovski
To allow both installation of IBX DB Control Grid and and this port

}
unit dbcntrlgrid;

{$mode objfpc}{$H+}
//{$DEFINE dbgDBCntrlGrid}
//{$DEFINE CONSOLEDEBUG}
interface

uses
  {$IFDEF CONSOLEDEBUG}
  {$IFDEF WINDOWS} Windows, {$ENDIF}
  {$ENDIF}
  Classes, Controls, SysUtils, types, DB, DBCtrls, Grids, LazLoggerBase, DBGrids, Graphics, StdCtrls,
  LMessages, LResources, ExtCtrls;

{
  The TRowCache is where we keep track of the DataSet and cache images of each row.
  TDBCntrlGrid is really a slight of hand. Only the active record is shown in
  the panel and the others are cached and displayed as images.

  The image cache is indexed by TDataSet.RecNo and accessed by current active
  record number (the data being displayed on the panel) and row offset from this
  record number.

  This is efficient but gives us a problem as the TDataSet model does not remove
  deleted records. Instead it simply marks them as deleted. Likewise, we need to
  keep track of deleted rows and skip over them when accessing the cache.

  When alternate row colours are in use, the cache is also used to keep track of the
  correct row colour as we must similarly ignore delete rows when calculating the
  correct colour. Odd and Even row numbers is not good enough here.
}


type
  TRowCacheState = (rcEmpty,rcPresent,rcDeleted);
  TRowDetails = record
    FState: TRowCacheState;
    FAlternateColor: boolean;
    FBitmap: TBitmap;
    aRecNo : Integer;
    aRecKey : Variant;

end;


type
  { TRowCache }

  TRowCache = class
  private


  private
    FAltColorStartNormal: boolean;
    FHeight: integer;
    FList: array of TRowDetails;
    aRowToRecNoArr : TIntegerDynArray;
    FUseAlternateColors: boolean;
    FWidth: integer;
    procedure FreeImages(Reset: boolean);
    function GetAlternateColor(RecNo: integer): boolean;
    procedure ExtendCache(aMaxIndex: integer);
    procedure OnWidthChange(Sender: TObject);
    procedure SetHeight(AValue: integer);
    procedure SetUseAlternateColors(AValue: boolean);
    procedure SetWidth(AValue: integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearCache;
    function Render(Control: TWinControl ): TBitmap;
    function Add2Cache(RecNo: Longint; Control: TWinControl; RecKey: Variant ): TBitmap;
    function GetRowImage(RecNo, aRow: Longint; KeyFieldValue: Variant): TBitmap;
    procedure InvalidateRowImage(RecNo: integer);
    function IsEmpty(RecNo: integer): boolean;
    procedure MarkAsDeleted(RecNo: integer);
    property AlternateColor[RecNo: integer]: boolean read GetAlternateColor;
    property Width: integer read FWidth write SetWidth;
    property Height: integer read FHeight write SetHeight;
    property AltColorStartNormal: boolean read FAltColorStartNormal write FAltColorStartNormal;
    property UseAlternateColors: boolean read FUseAlternateColors write SetUseAlternateColors;
  end;

  { TDBCntrlGridDataLink }

  TDBCntrlGridDataLink = class(TComponentDataLink)
  private
    FOnCheckBrowseMode: TDataSetNotifyEvent;
  protected
    procedure CheckBrowseMode; override;

  public
    property OnCheckBrowseMode: TDataSetNotifyEvent read FOnCheckBrowseMode write FOnCheckBrowseMode;
  end;

  TKeyDownHandler = procedure (Sender: TObject; var Key: Word;  Shift: TShiftState; var Done: boolean) of object;

  TPanelGridOption = (dgpIndicator,dgpDisableInsert,dgpCancelOnExit);
  TPanelGridOptions = set of TPanelGridOption;

  { TDBCntrlGrid }

  TDBCntrlGrid = class(TCustomGrid)
  private
    { Private declarations }
    FDataLink: TDBCntrlGridDataLink;
    FDefaultPositionAtEnd: boolean;
    FDrawPanel: TWinControl;
    FDrawingActiveRecord: boolean;
    FOldPosition: Integer;
    FOnKeyDownHander: TKeyDownHandler;
    fOnUpdateActive: TNotifyEvent;
    FOptions: TPanelGridOptions;
    FWeHaveFocus: boolean;
    FRowCache: TRowCache;
    FDrawRow: integer;          {The current row in the draw panel}
    FSelectedRow: integer;      {The row containing the current selection}
    FSelectedRecNo: integer;    {The DataSet RecNo for the current row}
    FRequiredRecNo: integer;    {Used after a big jump and is the dataset recno
                                 that we want to end up with}
    FModified: boolean;
    FLastRecordCount: integer;
    fResizeTimer : TTimer;
    {Used to pass mouse clicks to panel when focused row changes}
    FLastMouse: TPoint;
    FLastMouseButton: TMouseButton;
    FLastMouseShiftState: TShiftState;
    fSelectedRowIndicatorColor : TColor;
    fSaveAfterScrollHandler : TDataSetNotifyEvent;
    FDrawingEmptyDataset: Boolean;
    fKeyField : String;

    procedure ScheduleRefresh;
    function ActiveControl: TControl;
    procedure EmptyGrid;
    function GetDataSource: TDataSource;
    function GetRecordCount: Integer;
    procedure GetScrollbarParams(out aRange, aPage, aPos: Integer);
    function  GridCanModify: boolean;
    procedure DoDrawRow(aRow: integer; aRect: TRect; aState: TGridDrawState);
    procedure DoSelectNext(Data: PtrInt);
    procedure DoSendMouseClicks(Data: PtrInt);
    procedure KeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnRecordChanged(Field:TField);
    procedure OnCheckBrowseMode(aDataSet: TDataSet);
    procedure OnDataSetChanged(aDataSet: TDataSet);
    procedure OnDataSetOpen(aDataSet: TDataSet);
    procedure OnDataSetClose(aDataSet: TDataSet);
    procedure OnDrawPanelResize(Sender: TObject);
    procedure OnEditingChanged(aDataSet: TDataSet);
    procedure OnInvalidDataSet(aDataSet: TDataSet);
    procedure OnInvalidDataSource(aDataSet: TDataset);
    procedure OnLayoutChanged(aDataSet: TDataSet);
    procedure OnNewDataSet(aDataSet: TDataset);
    procedure OnDataSetScrolled(aDataSet:TDataSet; Distance: Integer);
    procedure OnUpdateData(aDataSet: TDataSet);
    procedure SetDataSource(AValue: TDataSource);
    procedure SetDrawPanel(AValue: TWinControl);
    procedure SetOptions(AValue: TPanelGridOptions);
    procedure SetSelectedRowIndicatorColor(AValue: TColor);
    procedure SetupDrawPanel(aRow: integer);
    function  UpdateGridCounts: Integer;
    procedure UpdateBufferCount;
    procedure UpdateDrawPanelBounds(aRow: integer);
    procedure UpdateScrollbarRange;
    procedure WMVScroll(var Message : TLMVScroll); message LM_VScroll;
    function  ISEOF: boolean;
    function  ValidDataSet: boolean;
    function  InsertCancelable: boolean;
    procedure ResizeTitemTimer( Sender : TObject );
  protected
    { Protected declarations }
    function  GetBufferCount: integer; virtual;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DoGridResize;
    procedure DoOnResize; override;
    procedure DrawAllRows; override;
    procedure DrawRow(ARow: Integer); override;
    procedure DrawCell(aCol,aRow: Integer; aRect: TRect; aState:TGridDrawState); override;
    procedure DrawIndicator(ACanvas: TCanvas; aRow: integer; R: TRect; Opt: TDataSetState; MultiSel: boolean); virtual;
    procedure GridMouseWheel(shift: TShiftState; Delta: Integer); override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure LinkActive(Value: Boolean); virtual;
    procedure LayoutChanged; virtual;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MoveSelection; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PrepareCanvas(aCol,aRow: Integer; aState:TGridDrawState); override;
    procedure ResetSizes; override;
    procedure SetColor(Value: TColor); override;
    procedure UpdateActive; virtual;
    procedure UpdateData; virtual;
    procedure UpdateShowing; override;
    procedure UpdateVertScrollbar(const aVisible: boolean; const aRange,aPage,aPos: Integer); override;
    procedure DoOnChangeBounds; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function MouseToRecordOffset(const x, y: Integer; out RecordOffset: Integer
      ): TGridZone;
    function ExecuteAction(AAction: TBasicAction): Boolean; override;
    function UpdateAction(AAction: TBasicAction): Boolean; override;
    property Datalink: TDBCntrlGridDataLink read FDatalink;
    procedure RefreshImagesCycle;
    //procedure Init( DS : TDataSource );
  published
    { Published declarations }
    property Align;
    property AlternateColor;
    property AltColorStartNormal;
    property Anchors;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
    property CellHintPriority;
    property Color;
    property Constraints;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DefaultPositionAtEnd: boolean read  FDefaultPositionAtEnd write FDefaultPositionAtEnd;
    property DragCursor;
    property DragMode;
    property DrawPanel: TWinControl read FDrawPanel write SetDrawPanel;
    property Enabled;
    property FixedColor;
    property FixedCols;
    property Flat;
    property Font;
    property Options: TPanelGridOptions read FOptions write SetOptions;
    property ParentBiDiMode;
    property ParentColor default false;
    property ParentFont;
    property PopupMenu;
    property Scrollbars default ssAutoVertical;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseXORFeatures;
    property Visible;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetCellHint;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnKeyDownHander: TKeyDownHandler read FOnKeyDownHander write FOnKeyDownHander;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPrepareCanvas;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property OnUpdateActive : TNotifyEvent read fOnUpdateActive Write FOnUpdateActive;
    Property SelectedRowIndicatorColor : TColor read fSelectedRowIndicatorColor Write SetSelectedRowIndicatorColor default clBtnFace;
    property KeyField : String Read fKeyField Write fKeyField;
end;


const
  COL_WIDTH = 20;
  FRAME_OFFSET = 8;

procedure Register;

implementation

uses LCLType, Math, LCLIntf, Forms, LCLMessageGlue, EditBtn, MaskEdit;

{ TDBCntrlGridDataLink }


{$ifdef dbgDBCntrlGrid}
function SBCodeToStr(Code: Integer): String;
begin
  Case Code of
    SB_LINEUP : result := 'SB_LINEUP';
    SB_LINEDOWN: result := 'SB_LINEDOWN';
    SB_PAGEUP: result := 'SB_PAGEUP';
    SB_PAGEDOWN: result := 'SB_PAGEDOWN';
    SB_THUMBTRACK: result := 'SB_THUMBTRACK';
    SB_THUMBPOSITION: result := 'SB_THUMBPOSITION';
    SB_ENDSCROLL: result := 'SB_SCROLLEND';
    SB_TOP: result := 'SB_TOP';
    SB_BOTTOM: result := 'SB_BOTTOM';
    else result :=IntToStr(Code)+ ' -> ?';
  end;
end;
{$endif}


procedure TDBCntrlGridDataLink.CheckBrowseMode;
begin
  inherited CheckBrowseMode;
  if assigned(FOnCheckBrowseMode) then
    OnCheckBrowseMode(DataSet);
end;

{ TRowCache }

procedure copyImage(form : TCustomControl; destination : TBitmap);
//procedure that assigns the screenshot of "form" to "destination"
var
tmp : TBitmap;
w,h : integer;
r : TRect;
begin
  w := form.Width;
  h:= form.Height;
  r:= Rect(0,0,w,h);
  tmp:=TBitmap.Create;
  try
  tmp.Width:=w;
  tmp.Height:=h;
  tmp.Canvas.CopyRect(r, form.Canvas, r);
  destination.Assign(tmp);
  finally
  tmp.Free;
  end;
end;


var ii : Integer = 0;
function TRowCache.Render(Control: TWinControl): TBitmap;
var Container: TBitmap;
begin
  Container := TBitmap.Create;
  try
    Container.SetSize(Control.Width,Control.Height);
    Container.Canvas.Brush.Color :=  control.Color;
    //Container.canvas.Brush.Style:=bsClear;
    Container.Canvas.FillRect(0,0,Control.Width,Control.Height);
    Control.PaintTo( Container.Canvas,0,0 );
    //{$ifdef WINDOWS}
    //Control.Invalidate;
    ////Application.ProcessMessages;
    //copyImage( TCustomControl( Control ), Container );
    //{$ELSE}
    //Control.PaintTo( Container.Canvas,0,0 );
    //{$ENDIF}
    //Container.SaveToClipboardFormat( PredefinedClipboardFormat(pcfBitmap) );
  except
    Container.Free;
    raise
  end;

  //if ssShift in GetKeyShiftState then begin
  //  Container.SaveToFile('c:\temp\'+ii.toString+'.bmp');
  //  inc (ii)
  //end;
  //
  Result := Container;

end;

procedure TRowCache.FreeImages(Reset: boolean);
var i: integer;
    altColor: boolean;
begin
  altColor := not AltColorStartNormal;
  for i := 0 to Length(FList) - 1 do
    begin
      if (FList[i].FState <> rcEmpty) and (FList[i].FBitmap <> nil) then
        begin
         FList[i].FBitmap.Free;
         FList[i].FBitmap := nil;
        end;
      if Reset or (FList[i].FState = rcPresent) then
        FList[i].FState := rcEmpty;
      if FList[i].FState <> rcDeleted then
      begin
        FList[i].FAlternateColor := altColor;
        altColor := not altColor;
      end;
    end;
end;

function TRowCache.GetAlternateColor(RecNo: integer): boolean;
begin
  ExtendCache(RecNo);
  Dec(RecNo);
  if (RecNo >= 0) and (RecNo < Length(FList)) then
    Result := FList[RecNo].FAlternateColor
  else
    Result := false;
end;

procedure TRowCache.ExtendCache(aMaxIndex: integer);
var i: integer;
    StartIndex: integer;
    altColor: boolean;
begin
  if aMaxIndex > Length(FList) then
  begin
     //aMaxIndex := aMaxIndex + 10;
    StartIndex := Length(FList);
    SetLength(FList,aMaxIndex);
    if not UseAlternateColors then
      altColor := false
    else
    if StartIndex = 0 then
       altColor := not AltColorStartNormal
    else
      altColor := not FList[StartIndex-1].FAlternateColor;

    for i := StartIndex to Length(FList) - 1 do
    begin
      FList[i].FState := rcEmpty;
      FList[i].FBitmap := nil;
      FList[i].FAlternateColor := altColor;
      if UseAlternateColors then
        altColor := not altColor;
    end;
  end;
end;

procedure TRowCache.OnWidthChange(Sender: TObject);
begin
  FreeImages(false);
end;

procedure TRowCache.SetHeight(AValue: integer);
begin
  if FHeight = AValue then Exit;
  FHeight := AValue;
  FreeImages(false);
end;

procedure TRowCache.SetUseAlternateColors(AValue: boolean);
begin
  if FUseAlternateColors = AValue then Exit;
  FUseAlternateColors := AValue;
  FreeImages(false);
end;

procedure TRowCache.SetWidth(AValue: integer);
begin
  if FWidth = AValue then Exit;
  FWidth := AValue;
  FreeImages(false);
end;

constructor TRowCache.Create;
begin
  SetLength(FList,0);
  SetLength(aRowToRecNoArr,0);
end;

destructor TRowCache.Destroy;
begin
  ClearCache;
  inherited Destroy;
end;

procedure TRowCache.ClearCache;
begin
  FreeImages(true);
  SetLength(FList,0);
  SetLength(aRowToRecNoArr,0);
end;

function TRowCache.Add2Cache(RecNo: Longint; Control: TWinControl;
  RecKey: Variant): TBitmap;
begin
  {$ifdef dbgDBCntrlGrid}
  DebugLn(ClassName,'.Add2Cache (RecNo='+RecNo.ToString+' Control='+Control.Name+') ');
  {$endif}
  Dec(RecNo); {Adust to zero base}
  ExtendCache(RecNo + 1);
  FList[RecNo].FState := rcPresent;
  if FList[RecNo].FBitmap <> nil then
    FList[RecNo].FBitmap.Free;
  FList[RecNo].FBitmap := Render(Control);
  FList[RecNo].aRecKey := RecKey;
  FList[RecNo].aRecNo := RecNo;
  //if ssShift in GetKeyShiftState then begin
  //  FList[RecNo].FBitmap .SaveToFile('c:\temp\'+(RecNo+1).toString+'-'+RecKey.ToString +'.bmp');
  //end;
  Result := FList[RecNo].FBitmap;
end;

function TRowCache.GetRowImage(RecNo, aRow: Longint; KeyFieldValue: Variant
  ): TBitmap;
var
  i: Integer;
  ff: TRowDetails;
begin
  Result := nil;
  {$ifdef dbgDBCntrlGrid}
  DebugLn(ClassName,'.GetRowImage(RecNo='+RecNo.ToString+' Offset='+Offset.ToString+' Length(FList)='+Length(FList).ToString+') ');
  {$endif}

  Dec(RecNo); {adjust to zero base}
  if (RecNo < 0) or (RecNo >= Length(FList)) then begin
    if KeyFieldValue<>NULL then begin
      for ff in FList do        // Try to find by Key(in a case of insert, RecNo is = 0)
        if ( ff.aRecKey = KeyFieldValue) and ( ff.FState = rcPresent ) then
          Exit( FF.FBitmap );
      end;

    if Length( aRowToRecNoArr ) > aRow then
      if aRowToRecNoArr[ aRow ] < Length( FList ) then
        if FList[ aRowToRecNoArr[ aRow ] ].FState = rcPresent then
          Exit( FList[ aRowToRecNoArr[ aRow ] ].FBitmap );
    Exit;
    end;

  if Length( aRowToRecNoArr ) <= aRow then
    SetLength( aRowToRecNoArr, aRow + 1 );
  aRowToRecNoArr[ aRow ] := RecNo;
  if FList[RecNo].FState = rcPresent then
     Result := FList[RecNo].FBitmap;

end;

procedure TRowCache.InvalidateRowImage(RecNo: integer);
begin
  {$ifdef dbgDBCntrlGrid}
  DebugLn(ClassName,'.InvalidateRowImage (RecNo='+RecNo.ToString+') ');
  {$endif}

  Dec(RecNo); {adjust to zero base}
  if (RecNo < 0) or (RecNo >= Length(FList)) then
    Exit;

  if FList[RecNo].FState = rcPresent then
  begin
    FList[RecNo].FBitmap.Free;
    FList[RecNo].FBitmap := nil;
    FList[RecNo].FState := rcEmpty;
  end;
end;

function TRowCache.IsEmpty(RecNo: integer): boolean;
begin
  Dec(RecNo);
  Result := (RecNo < 0) or (RecNo >= Length(FList)) or (FList[RecNo].FState = rcEmpty);
end;

procedure TRowCache.MarkAsDeleted(RecNo: integer);
var altColor: boolean;
    i: integer;
begin
  {$ifdef dbgDBCntrlGrid}
  DebugLn(ClassName,'.MarkAsDeleted (RecNo='+RecNo.ToString+') ');
  {$endif}
  Dec(RecNo); {adjust to zero base}
  if (RecNo < 0) or (RecNo >= Length(FList)) then
    Exit;

  FList[RecNo].FState := rcDeleted;
  if not UseAlternateColors then
    Exit;

  {Reset Alternate Colours}

  if RecNo = 0 then
     altColor := not AltColorStartNormal
  else
    altColor := not FList[RecNo-1].FAlternateColor;

  for i := RecNo + 1 to Length(FList) - 1 do
  begin
    if FList[i].FState <> rcDeleted then
    begin
      FList[i].FAlternateColor := altColor;
      altColor := not altColor;
      if FList[i].FState = rcPresent then
      begin
        FList[i].FBitmap.Free;
        FList[i].FState := rcEmpty;
      end;
    end;
  end;
end;

{ TDBCntrlGrid }

procedure TDBCntrlGrid.ScheduleRefresh;
begin
  fResizeTimer.Enabled := False;
  fResizeTimer.Enabled := True;

end;

function TDBCntrlGrid.ActiveControl: TControl;
var AParent: TWinControl;
begin
  Result := nil;
  AParent := Parent;
  while (AParent <> nil) and  not (AParent is TCustomForm) do
    AParent := AParent.Parent;
  if (AParent <> nil) and (AParent is TCustomForm)then
      Result := TCustomForm(AParent).ActiveControl;
end;

procedure TDBCntrlGrid.EmptyGrid;
var
  OldFixedRows: Integer;
begin
  {$ifdef dbgDBCntrlGrid}
  DebugLn(ClassName,'.EmptyGrid ');
  {$endif}
  OldFixedRows := FixedRows;
  Clear;
  FRowCache.ClearCache;
  RowCount := OldFixedRows + 1;
  if dgpIndicator in FOptions then
    ColWidths[0]:=Scale96ToFont(COL_WIDTH);
  if assigned(FDrawPanel) then
    FDrawPanel.Visible := false;
end;

function TDBCntrlGrid.GetDataSource: TDataSource;
begin
  Result:= FDataLink.DataSource;
end;

function TDBCntrlGrid.GetRecordCount: Integer;
begin
  if assigned(FDataLink.DataSet) then
    result := FDataLink.DataSet.RecordCount
  else
    result := 0;
  {$ifdef dbgDBCntrlGrid}
  DebugLn(ClassName,'.GetRecordCount Result='+result.ToString);
  {$endif}
end;

procedure TDBCntrlGrid.GetScrollbarParams(out aRange, aPage, aPos: Integer);
begin
  if (FDatalink<>nil) and (FDataLink.DataSet <> nil) and FDatalink.Active then begin
    if FDatalink.dataset.IsSequenced then begin
      aRange := GetRecordCount + VisibleRowCount - 1;
      aPage := VisibleRowCount;
      if aPage<1 then aPage := 1;
      if FDatalink.BOF then aPos := 0 else
      if FDatalink.EOF then aPos := aRange
      else
        aPos := FDataLink.DataSet.RecNo - 1; // RecNo is 1 based
      if aPos<0 then aPos:=0;
    end else begin
      aRange := 6;
      aPage := 2;
      if FDatalink.EOF then aPos := 4 else
      if FDatalink.BOF then aPos := 0
      else aPos := 2;
    end;
  end else begin
    aRange := 0;
    aPage := 0;
    aPos := 0;
  end;
end;

function TDBCntrlGrid.GridCanModify: boolean;
begin
  result := not FDataLink.ReadOnly
    and ValidDataSet and FDatalink.DataSet.CanModify;
end;

procedure TDBCntrlGrid.DoDrawRow(aRow: integer; aRect: TRect;
  aState: TGridDrawState);
var CachedRow: TBitmap;
  Cr : String;
  Rn,Rn1: LongInt;
  cmp: TComponent;
begin
  {$IFDEF CONSOLEDEBUG}
  WriteLn('DoDrawRow received, arow=',arow);
  {$ENDIF}
  CachedRow := nil;
  if Assigned( fDatalink.DataSet ) then
    CachedRow := FRowCache.GetRowImage( fDatalink.DataSet.RecNo,aRow, fDatalink.DataSet.FieldByName(KeyField).Value );

  {if the row is in the cache then draw it - otherwise schedule a cache refresh cycle}
  if CachedRow = nil then begin
    if Assigned( Datalink.DataSet ) and (fDatalink.DataSet.RecNo > 0) and ( FSelectedRecNo <> Datalink.DataSet.RecNo )  then begin

      //TPanel(FDrawPanel).Canvas.TextOut(10,30,fDatalink.DataSet.RecNo.ToString);
      //FRowCache.Add2Cache(fDatalink.DataSet.RecNo, FDrawPanel );

      // Mark image as missing;
      FRowCache.InvalidateRowImage( fDatalink.DataSet.RecNo );

      //CachedRow := FRowCache.GetRowImage(fDatalink.DataSet.RecNo,aRow);
      {$IFDEF CONSOLEDEBUG}
      WriteLn('Cache Build, FSelectedRecNo= RecNo=',FSelectedRecNo,',',Datalink.DataSet.RecNo);
      {$ENDIF}
      end
    else
      begin
      {$IFDEF CONSOLEDEBUG}
      WriteLn(LineEnding+'CachedRow NOT BUILT FSelectedRecNo= RecNo=',FSelectedRecNo,',',Datalink.DataSet.RecNo);
      {$ENDIF}
      end;
    end;
   if Assigned( CachedRow ) then
     begin
     Canvas.FillRect(aRect);
     Canvas.Draw(aRect.Left,aRect.Top+Scale96ToScreen( FRAME_OFFSET ),CachedRow);
     //CachedRow.SaveToFile('C:\temp\'+fDataLink.DataSet.Fields[ 0 ].AsString+'.bmp' );
     {$IFDEF CONSOLEDEBUG}
     if fDatalink.DataSet.ControlsDisabled then
       Canvas.TextOut( aRect.Left,aRect.Top+Scale96ToScreen( FRAME_OFFSET ),'DISABLED!'+fDataLink.DataSet.Fields[ 0 ].AsString+' aRow='+aRow.ToString+' RecNo='+fDatalink.DataSet.RecNo.ToString )
     else
       Canvas.TextOut( aRect.Left,aRect.Top+Scale96ToScreen( FRAME_OFFSET ),fDataLink.DataSet.Fields[ 0 ].AsString+' aRow='+aRow.ToString+' RecNo='+fDatalink.DataSet.RecNo.ToString );
     WriteLn('aRow=',arow,' Field[ 0 ]=',fDataLink.DataSet.Fields[ 0 ].AsString );
     {$ENDIF}
     end
   else
     begin
     Canvas.FillRect(aRect);
     if fDataLink.DataSet.State in dsEditModes then begin
       Canvas.Pen.Color := SelectedRowIndicatorColor;
       Canvas.Pen.Style := psDashDot;
       aRect.Left += 1;
       aRect.Right -= 1;
       aRect.Top += 1;
       aRect.Bottom -= 1;
       Canvas.Rectangle( aRect );
       Canvas.MoveTo(aRect.Left,aRect.Top);
       Canvas.LineTo(aRect.Right,aRect.Bottom);
       Canvas.MoveTo(aRect.Left,aRect.Bottom);
       Canvas.LineTo(aRect.Right,aRect.Top);
       end;
     ScheduleRefresh;

     {$IFDEF CONSOLEDEBUG}
     WriteLn('CachedRow =nil, Field[ 0 ]=',fDataLink.DataSet.Fields[ 0 ].AsString );
     if fDataLink.DataSet.State = dsInsert then
       Canvas.TextOut( aRect.Left,aRect.Top+Scale96ToScreen( FRAME_OFFSET ),fDataLink.DataSet.Fields[ 0 ].AsString +' - DSINSERT! CachedRow=nil!!!'  )
     else
       Canvas.TextOut( aRect.Left,aRect.Top+Scale96ToScreen( FRAME_OFFSET ),fDataLink.DataSet.Fields[ 0 ].AsString +' - CachedRow=nil!!!'  );
     {$ENDIF}
     end;
end;


procedure TDBCntrlGrid.DoSendMouseClicks(Data: PtrInt);
var P: TPoint;
    Control: TControl;
begin
  if AppDestroying in Application.Flags then Exit;

  if assigned(FDrawPanel) and (FLastMouse.X <> 0) then
  begin
    P := ClientToScreen(FLastMouse);
    Control :=  FindControlAtPosition(P,false);
    if (Control <> nil) and (Control is TWinControl) then
      TWinControl(Control).SetFocus
    else
      Control := FDrawPanel;

    P := Control.ScreenToClient(P);

    LCLSendMouseDownMsg(Control,P.X,P.Y,FLastMouseButton,FLastMouseShiftState);
    LCLSendMouseUpMsg(Control,P.X,P.Y, FLastMouseButton,FLastMouseShiftState);

  end;
  FLastMouse.X := 0;
end;

procedure TDBCntrlGrid.KeyDownHandler(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var Done: boolean;
    AControl: TControl;
begin
  if Visible and assigned(FDrawPanel) and FDrawPanel.Visible and FWeHaveFocus and (Self.Owner=Screen.ActiveForm) then
  begin
    AControl := ActiveControl;
    if (AControl <> nil) and (AControl is TCustomComboBox)
                         and ((Key in [VK_UP,VK_DOWN]) or
                         (TCustomComboBox(AControl).DroppedDown and (Key = VK_RETURN)) or
                         ((TCustomComboBox(AControl).Text <> '') and (Key =  VK_ESCAPE))) then
      Exit; {ignore these keys if we are in a  combobox}

    if (AControl <> nil) and (AControl is TCustomMemo)
                         and (Key in [VK_RETURN,VK_UP,VK_DOWN]) then Exit; {Ignore Return in a CustomMemo}

    if (AControl <> nil) and (AControl is TCustomGrid)
                         and (Key in [VK_RETURN,VK_UP,VK_DOWN,VK_TAB]) then Exit; {Ignore Return in a CustomMemo}

    if (AControl <> nil) and ((AControl is TDateEdit) or (AControl is TCustomMaskedit))
                         and (Key in [VK_RETURN,VK_UP,VK_DOWN,
                               VK_ESCAPE,VK_LEFT,VK_RIGHT]) then Exit; {Ignore Return in a CustomMemo}
    Done := false;
    if assigned(FOnKeyDownHander) then
      OnKeyDownHander(Sender,Key,Shift,Done);
    if Done then Exit;

    KeyDown(Key,Shift)
  end;
end;

procedure TDBCntrlGrid.OnRecordChanged(Field: TField);
begin
  {$ifdef dbgDBCntrlGrid}
  DebugLn(ClassName,'.OnRecordChanged(Field=');
  if Field=nil then DebugLn('nil)')
  else              DebugLn(Field.FieldName,')');
  {$endif}
  UpdateActive
end;

procedure TDBCntrlGrid.OnCheckBrowseMode(aDataSet: TDataSet);
var RecNo: integer;
begin
  {$ifdef dbgDBCntrlGrid}
  DbgOut(ClassName,'.OnCheckBrowseMode(RecNo=');
  DebugLn(aDataSet.RecNo.tostring,')');
  {$endif}
  if assigned(FDrawPanel) and (aDataSet.RecNo > 0)
      and (FModified or (FRowCache.IsEmpty(aDataSet.RecNo))) then
  begin
    RecNo := aDataSet.RecNo;
    {$ifdef LINUX}
    Application.ProcessMessages;
    {$ENDIF}
    if (RecNo = aDataSet.RecNo) and ( KeyField<>'' ) then   {Guard against sudden changes}
      FRowCache.Add2Cache(RecNo, FDrawPanel, aDataSet.FieldByName(KeyField).Value );
  end;
end;

procedure TDBCntrlGrid.OnDataSetChanged(aDataSet: TDataSet);
begin
  {$ifdef dbgDBCntrlGrid}
  DebugLnEnter('%s.OnDataSetChanged INIT name=%s aDataSet=%s',
  	[ClassName,name,dbgsname(ADataset)]);
  {$endif}

  if ( KeyField = '' ) and ValidDataSet then
    if FDataLink.DataSource.DataSet.FieldCount > 0  then
      KeyField := FDataLink.DataSource.DataSet.Fields[ 0 ].FieldName;

  LayoutChanged;
  UpdateActive;
  {$ifdef dbgDBCntrlGrid}
  DebugLnExit('%s.OnDataSetChanged DONE name=%s aDataSet=%s',
  	[ClassName,name,dbgsname(ADataset)]);
  {$endif}
end;

procedure TDBCntrlGrid.OnDataSetOpen(aDataSet: TDataSet);
begin
  {$ifdef dbgDBCntrlGrid}
  DebugLnEnter('%s.OnDataSetOpen INIT', [ClassName]);
  {$endif}
  LinkActive(true);
  UpdateActive;
  {$ifdef dbgDBCntrlGrid}
  DebugLnExit('%s.OnDataSetOpen DONE', [ClassName]);
  {$endif}
end;

procedure TDBCntrlGrid.OnDataSetClose(aDataSet: TDataSet);
begin
  {$ifdef dbgDBCntrlGrid}
  DebugLn('%s.OnDataSetClose', [ClassName]);
  {$endif}
  LinkActive(false);
end;

procedure TDBCntrlGrid.OnDrawPanelResize(Sender: TObject);
begin
  FRowCache.Height := FDrawPanel.Height;
  DefaultRowHeight := FDrawPanel.Height + Scale96ToScreen( FRAME_OFFSET * 2 );
  {$ifdef dbgDBCntrlGrid}
  DbgOut(ClassName,'.OnDrawPanelResize(Height=');
  DebugLn(FDrawPanel.Height.tostring,')');
  {$endif}
end;

procedure TDBCntrlGrid.OnEditingChanged(aDataSet: TDataSet);
begin
  {$ifdef dbgDBCntrlGrid}
  DebugLn('%s.OnEditingChanged', [ClassName]);
  if aDataSet<>nil then begin
    DebugLn(['Editing=', dsEdit = aDataSet.State]);
    DebugLn(['Inserting=',dsInsert = aDataSet.State]);
  end else
    DebugLn('Dataset=nil');
  {$endif}
  FModified := true;
  UpdateActive;
end;

procedure TDBCntrlGrid.OnInvalidDataSet(aDataSet: TDataSet);
begin
  {$ifdef dbgDBCntrlGrid}
  DebugLn('%s.OnInvalidDataSet', [ClassName]);
  {$endif}
  LinkActive(False);
end;

procedure TDBCntrlGrid.OnInvalidDataSource(aDataSet: TDataset);
begin
  {$ifdef dbgDBCntrlGrid}
  DebugLn('%s.OnInvalidDataSource', [ClassName]);
  {$endif}
  LinkActive(False);
end;

procedure TDBCntrlGrid.OnLayoutChanged(aDataSet: TDataSet);
begin
  {$ifdef dbgDBCntrlGrid}
  DebugLn('%s.OnLayoutChanged', [ClassName]);
  {$endif}
  LayoutChanged;
end;

procedure TDBCntrlGrid.OnNewDataSet(aDataSet: TDataset);
begin
  {$ifdef dbgDBCntrlGrid}
  DebugLnEnter('%s.OnNewDataSet INIT', [ClassName]);
  {$endif}
  LinkActive(True);
  UpdateActive;
  {$ifdef dbgDBCntrlGrid}
  DebugLnExit('%s.OnNewDataSet DONE', [ClassName]);
  {$endif}
end;

procedure TDBCntrlGrid.OnDataSetScrolled(aDataSet: TDataSet; Distance: Integer);
var
  OldRow: Integer;
begin
  {$ifdef dbgDBCntrlGrid}
  DebugLn('%s.OnDataSetScrolled Distance=%d ds.RecordCount=%d',[ClassName, Distance, aDataSet.RecordCount]);
  {$endif}
  UpdateScrollBarRange;
  if Distance <> 0 then
  begin
    FDrawRow := FixedRows + FDataLink.ActiveRecord;

    OldRow := Row;
    Row := FixedRows + FDataLink.ActiveRecord;
    if OldRow=Row then  // if OldRow<>NewRow SelectEditor will be called by MoveExtend
      SetupDrawPanel(FDrawRow);     // if OldRow=NewRow we need to manually call SelectEditor

    Invalidate;

  end
  else
     UpdateActive;


  //var
  //  OldEditorMode: boolean;
  //  OldRow: Integer;
  //begin
  //  {$ifdef dbgDBGrid}
  //  DebugLn('%s.OnDataSetScrolled Distance=%d ds.RecordCount=%d',[ClassName, Distance, aDataSet.RecordCount]);
  //  {$endif}
  //  UpdateScrollBarRange;
  //  // todo: Use a fast interface method to scroll a rectangular section of window
  //  //       if distance=+, Row[Distance] to Row[RowCount-2] UP
  //  //       if distance=-, Row[FixedRows+1] to Row[RowCount+Distance] DOWN
  //
  //  OldEditorMode := EditorMode;
  //  if OldEditorMode then
  //    EditorMode := False;
  //
  //  if Distance<>0 then begin
  //
  //    OldRow := Row;
  //    Row := FixedRows + FDataLink.ActiveRecord;
  //    if OldRow=Row then  // if OldRow<>NewRow SelectEditor will be called by MoveExtend
  //      SelectEditor;     // if OldRow=NewRow we need to manually call SelectEditor
  //
  //    Invalidate;
  //  end else
  //    UpdateActive;
  //
  //  if OldEditorMode and (dgAlwaysShowEditor in Options) then
  //    EditorMode := True;




end;

procedure TDBCntrlGrid.OnUpdateData(aDataSet: TDataSet);
begin
  {$ifdef dbgDBCntrlGrid}
  DebugLn('%s.OnUpdateData', [ClassName]);
  {$endif}
  UpdateData;
end;

procedure TDBCntrlGrid.SetDataSource(AValue: TDataSource);
begin
  {$ifdef dbgDBCntrlGrid}
  DebugLn('%s.SetDataSource', [ClassName]);
  {$endif}
  if AValue = FDatalink.Datasource then Exit;
  FDataLink.DataSource := AValue;
  if Assigned(FDatalink.DataSet) then
    if KeyField = '' then
      if FDataLink.DataSource.DataSet.FieldCount > 0  then
        KeyField := FDataLink.DataSource.DataSet.Fields[ 0 ].FieldName;
  UpdateActive;
end;

procedure TDBCntrlGrid.SetDrawPanel(AValue: TWinControl);
var theForm: TWinControl;
begin
  {$ifdef dbgDBCntrlGrid}
  DbgOut(ClassName,'.SetDrawPanel(RecNo=');
  if Assigned(DataSource) and Assigned(DataSource.DataSet) then
    DebugLn(DataSource.DataSet.RecNo.tostring,')')
  else
    DebugLn('NULL',')');
  {$endif}
  if FDrawPanel = AValue then Exit;
  if FDrawPanel <> nil then
  begin
     RemoveFreeNotification(FDrawPanel);
     FDrawPanel.RemoveAllHandlersOfObject(self);
     theForm := Parent;
     while not ((theForm is TCustomForm) or (theForm is TCustomFrame))
                           and (theForm.Parent <> nil) do
       theForm := theForm.Parent;
     FDrawPanel.Parent := theForm;
  end;
  FRowCache.ClearCache;
  try
    FDrawPanel := AValue;
    if assigned(FDrawPanel) then
    begin
      FDrawPanel.Parent := self;
      DefaultRowHeight := FDrawPanel.Height + Scale96ToScreen( FRAME_OFFSET * 2 );
      if csDesigning in ComponentState then
        UpdateDrawPanelBounds(0)
      else
       FDrawPanel.Visible := false;
      FRowCache.Height := FDrawPanel.Height;
      FRowCache.Width := FDrawPanel.Width;
      FDrawPanel.AddHandlerOnResize(@OnDrawPanelResize);
      FreeNotification(FDrawPanel);
    end;
  except
    FDrawPanel := nil;
    raise;
  end;
end;

procedure TDBCntrlGrid.SetOptions(AValue: TPanelGridOptions);
begin
  {$ifdef dbgDBCntrlGrid}
  DebugLnEnter('%s.SetOptions INIT', [ClassName]);
  {$endif}
  if FOptions = AValue then Exit;
  FOptions := AValue;
  if dgpIndicator in FOptions then
  begin
    FixedCols := 1;
    ColWidths[0] := Scale96ToFont(COL_WIDTH)
  end
  else
    FixedCols := 0;
  {$ifdef dbgDBCntrlGrid}
  DebugLnExit('%s.SetOptions DONE', [ClassName]);
  {$endif}
end;

procedure TDBCntrlGrid.SetSelectedRowIndicatorColor(AValue: TColor);
begin
  if fSelectedRowIndicatorColor = AValue then Exit;
  fSelectedRowIndicatorColor := AValue;
  Invalidate;
end;

procedure TDBCntrlGrid.SetupDrawPanel(aRow: integer);
begin
  {$ifdef dbgDBCntrlGrid}
  DbgOut(ClassName,'.SetupDrawPanel(RecNo=');
  if Assigned(DataSource) and Assigned(DataSource.DataSet) then
    DebugLn(dataSource.DataSet.RecNo.tostring,') aRow=('+aRow.tostring+')')
  else
    DebugLn('NULL',') aRow=('+aRow.tostring+')');
  {$endif}
  if FDrawPanel = nil then Exit;
  {$IFDEF CONSOLEDEBUG}
  WriteLn( 'SetupDrawPanel aRow=',aRow );
  {$ENDIF}
  if ValidDataSet and FRowCache.AlternateColor[FDataLink.DataSet.RecNo] then
    FDrawPanel.Color := AlternateColor
  else
    FDrawPanel.Color := self.Color;
  FDrawPanel.Visible := true;

  UpdateDrawPanelBounds(aRow);         {Position Draw Panel over expanded Row}
  Invalidate;
end;

function TDBCntrlGrid.UpdateGridCounts: Integer;
var
  RecCount: Integer;
  FRCount, FCCount: Integer;
begin
  BeginUpdate;
  try
    FRCount := 0;
    if dgpIndicator in FOptions then
       FCCount := 1
    else
      FCCount := 0;
      if FDataLink.Active then begin
        UpdateBufferCount;
        RecCount := FDataLink.RecordCount;
        if RecCount<1 then
          RecCount := 1;
      end else begin
        RecCount := 0;
        if FRCount=0 then
          // need to be large enough to hold indicator
          // if there is one, and if there are no titles
          RecCount := FCCount;
      end;

      Inc(RecCount, FRCount);

      RowCount := RecCount;
      FixedRows := FRCount;
      Result := RowCount ;
      if FDatalink.Active and (FDatalink.ActiveRecord>=0) then
        AdjustEditorBounds(Col, FixedRows + FDatalink.ActiveRecord);
    {$ifdef dbgDBCntrlGrid}
    DbgOut(ClassName,'.UpdateGridCounts(RecNo=');
    if Assigned(DataSource) and Assigned(DataSource.DataSet) then
      DebugLn(DataSource.DataSet.RecNo.tostring,') RowCount=(' +RowCount.tostring,') FixedRows=('+FixedRows.tostring+')')
    else
      DebugLn('NULL',') RowCount=(' +RowCount.tostring,') FixedRows=('+FixedRows.tostring+')');
    {$endif}
  finally
    EndUpdate;
  end;
end;

procedure TDBCntrlGrid.UpdateBufferCount;
var
  BCount: Integer;
begin
  BCount := -1;
  if FDataLink.Active then begin
    BCount := GetBufferCount;
    if BCount<1 then
      BCount := 1;
    FDataLink.BufferCount:= BCount;
  end;
  {$ifdef dbgDBCntrlGrid}
  DbgOut(ClassName,'.UpdateBufferCount(RecNo=');

  if Assigned(DataSource) and Assigned(DataSource.DataSet) then
    DebugLn(DataSource.DataSet.RecNo.tostring,') BCount=('+BCount.tostring+')')
  else
    DebugLn('NULL',') BCount=('+BCount.tostring+')');
  {$endif}
end;

procedure TDBCntrlGrid.UpdateDrawPanelBounds(aRow: integer);
var R: TRect;
begin
  R := Rect(0,0,0,0);
  if assigned(FDrawPanel) and
   (aRow >= 0) and (aRow < RowCount) then
  begin
    // Upper and Lower bounds for this row
    ColRowToOffSet(False, True, aRow, R.Top, R.Bottom);
    //Bounds for visible Column
    ColRowToOffSet(True,True,ColCount-1,R.Left,R.RIght);
    r.top := r.top + Scale96ToScreen( FRAME_OFFSET );
    r.Bottom := r.Bottom - Scale96ToScreen( FRAME_OFFSET );
    r.Right := r.Right - Scale96ToScreen( FRAME_OFFSET );
    FDrawPanel.BoundsRect := R;
    {$ifdef dbgDBCntrlGrid}
    DbgOut(ClassName,'.UpdateDrawPanelBounds(RecNo=');
    if Assigned(DataSource) and Assigned(DataSource.DataSet) then
      DebugLn(DataSource.DataSet.RecNo.tostring,') aRow=('+aRow.tostring+')')
    else
      DebugLn('NULL',') aRow=('+aRow.tostring+')');
    {$endif}
  end;
end;

procedure TDBCntrlGrid.UpdateScrollbarRange;
var
  aRange, aPage, aPos: Integer;
  ScrollInfo: TScrollInfo;
begin

  if not HandleAllocated then exit;
  {$ifdef dbgDBCntrlGrid}
  DebugLnEnter('%s.UpdateScrollbarRange INIT', [ClassName]);
  {$endif}

  GetScrollBarParams(aRange, aPage, aPos);

  FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
  ScrollInfo.cbSize := SizeOf(ScrollInfo);

  {TODO: try to move this out}
  {$ifdef WINDOWS}
  ScrollInfo.fMask := SIF_ALL or SIF_DISABLENOSCROLL;
  ScrollInfo.ntrackPos := 0;
  {$else}
  ScrollInfo.fMask := SIF_ALL or SIF_UPDATEPOLICY;
  //ScrollInfo.ntrackPos := SB_POLICY_CONTINUOUS;
  ScrollInfo.ntrackPos := SB_POLICY_DISCONTINUOUS;
  {$endif}
  ScrollInfo.nMin := 0;
  ScrollInfo.nMax := aRange;
  ScrollInfo.nPos := Min(aPos,aRange-aPage);
  ScrollInfo.nPage := aPage;
  // the redraw argument of SetScrollInfo means under gtk
  // if the scrollbar is visible or not, in windows it
  // seems to mean if the scrollbar is redrawn or not
  // to reflect the scrollbar changes made
  SetScrollInfo(Handle, SB_VERT, ScrollInfo,
    (ScrollBars in [ssBoth, ssVertical]) or
    ((Scrollbars in [ssAutoVertical, ssAutoBoth]) and (aRange>aPAge))
  );
  FOldPosition := aPos;
  {$ifdef dbgDBCntrlGrid}
  DebugLnExit('%s.UpdateScrollBarRange DONE Handle=%d aRange=%d aPage=%d aPos=%d',
    [ClassName, Handle, aRange, aPage, aPos]);
  {$endif}
end;

procedure TDBCntrlGrid.WMVScroll(var Message: TLMVScroll);
var
  IsSeq: boolean;
  aPos, aRange, aPage: Integer;
  DeltaRec: integer;

  function MaxPos: Integer;
  begin
    if IsSeq then
      result := GetRecordCount - 1
    else
      result := 4;
  end;

  procedure DsMoveBy(Delta: Integer);
  begin
    FDataLink.DataSet.MoveBy(Delta);
    {$IFDEF CONSOLEDEBUG}
    WriteLn( 'DSMoveBy Delta=',Delta );
    {$ENDIF}
    GetScrollbarParams(aRange, aPage, aPos);
  end;

  procedure DsGoto(BOF: boolean);
  begin
    if BOF then FDatalink.DataSet.First
    else        FDataLink.DataSet.Last;
    GetScrollbarParams(aRange, aPage, aPos);
  end;

  function DsPos: boolean;
  begin
    result := false;
    aPos := Message.Pos;
    if aPos=FOldPosition then begin
      result := true;
      exit;
    end;
    if aPos>=MaxPos then
      dsGoto(False)
    else if aPos<=0 then
      dsGoto(True)
    else if IsSeq then
      FDatalink.DataSet.RecNo := aPos + 1
    else begin
      DeltaRec := Message.Pos - FOldPosition;
      if DeltaRec=0 then begin
        result := true;
        exit
      end
      else if DeltaRec<-1 then
        DsMoveBy(-VisibleRowCount)
      else if DeltaRec>1 then
        DsMoveBy(VisibleRowCount)
      else
        DsMoveBy(DeltaRec);
    end;
  end;

begin
  if not FDatalink.Active or not assigned(FDataLink.DataSet) then exit;
  {$ifdef dbgDBCntrlGrid}
  DebugLnEnter('%s.WMVScroll INIT Code=%s Position=%s OldPos=%s VisibleRowCount=%d',
  			[ClassName, SbCodeToStr(Message.ScrollCode), dbgs(Message.Pos), Dbgs(FOldPosition),VisibleRowCount]);
  {$endif}
  IsSeq := FDatalink.DataSet.IsSequenced and not FDataLink.DataSet.Filtered;
  case Message.ScrollCode of
    SB_TOP:
      DsGoto(True);
    SB_BOTTOM:
      DsGoto(False);
    SB_PAGEUP:
      DsMoveBy(-VisibleRowCount);
    SB_LINEUP:
      DsMoveBy(-1);
    SB_LINEDOWN:
      DsMoveBy(1);
    SB_PAGEDOWN:
      DsMoveBy(VisibleRowCount);
    SB_THUMBPOSITION:
      if DsPos then
        exit;
    SB_THUMBTRACK:
        if not (FDatalink.DataSet.IsSequenced) or DsPos then
        begin
          {$ifdef dbgDBCntrlGrid}
          DebugLnExit('%s.WMVScroll EXIT: SB_THUMBTRACK: DsPos or not sequenced', [ClassName]);
          {$endif}
          exit;
        end;
    else begin
      {$ifdef dbgDBCntrlGrid}
      DebugLnExit('%s.WMVScroll EXIT: invalid ScrollCode: %d', [ClassName, message.ScrollCode]);
      {$endif}
      Exit;
    end;
  end;

  ScrollBarPosition(SB_VERT, aPos);
  FOldPosition:=aPos;  end;

function TDBCntrlGrid.ISEOF: boolean;
begin
  with FDatalink do
    result := ValidDataSet and DataSet.EOF;
  {$ifdef dbgDBCntrlGrid}
  DebugLn('%s.IsEOF Result=%s', [ClassName,Result.ToString(true)]);
  {$endif}
end;

function TDBCntrlGrid.ValidDataSet: boolean;
begin
   result := FDatalink.Active And (FDatalink.DataSet<>nil)
end;

function TDBCntrlGrid.InsertCancelable: boolean;
begin
  Result := ValidDataSet;
  if Result then
  with FDatalink.DataSet do
    Result := (State=dsInsert) and not Modified ;
  {$ifdef dbgDBCntrlGrid}
  DebugLn('%s.InsertCancelable Result=%s', [ClassName,Result.ToString(true)]);
  {$endif}
end;

procedure TDBCntrlGrid.ResizeTitemTimer(Sender: TObject);
begin
  if fResizetimer.Tag <> 0 then Exit;
  fResizetimer.Tag := 1;
  try
  if not ValidDataSet then Exit;
  if not ( FDataLink.DataSet.State  In dsEditModes ) then begin
    fResizetimer.Enabled := False;
    RefreshImagesCycle;
  end;  // If We Are in some edit mode - wait until it finished; then refresh.

  Finally
  fResizetimer.Tag := 0 ;
  end;
end;

function TDBCntrlGrid.GetBufferCount: integer;
begin
  Result := ClientHeight div DefaultRowHeight;
  {$ifdef dbgDBCntrlGrid}
  DebugLn('%s.GetBufferCount Result=%s', [ClassName,Result.ToString]);
  {$endif}
end;

procedure TDBCntrlGrid.DoEnter;
begin
  inherited DoEnter;
  FWeHaveFocus := true;
end;

procedure TDBCntrlGrid.DoExit;
begin
  FWeHaveFocus := false;
  if ValidDataSet and (dgpCancelOnExit in Options) and
    InsertCancelable then
  begin
    FDataLink.DataSet.Cancel;
  end;
  inherited DoExit;
end;

procedure TDBCntrlGrid.DoGridResize;
var
  fNeedRefresh: Boolean;
begin
    if Columns.Count = 0 then Exit;
    BeginUpdate;
    try

    if Columns.Count = 0 then Exit;

    if ColCount > 1 then
      Columns[0].Width := ClientWidth - ColWidths[0]
    else
      Columns[0].Width := ClientWidth;

    fNeedRefresh := FRowCache.Width <> Columns[0].Width;
    FRowCache.Width := Columns[0].Width;

    if fNeedRefresh then begin // Schedule refresh for a missing rows
      fResizeTimer.Enabled := False;
      fResizeTimer.Enabled := True;
    end;

    {$ifdef dbgDBCntrlGrid}
    DebugLn('%s.DoGridResize Width=%s', [ClassName,FRowCache.Width.ToString]);
    {$endif}
    UpdateDrawPanelBounds( Row );

    finally
    EndUpdate();
    end;

    Repaint;

end;

procedure TDBCntrlGrid.DoOnResize;
begin
  inherited DoOnResize;
  DoGridResize;
end;



procedure TDBCntrlGrid.DoSelectNext(Data: PtrInt);
begin
  FDataLink.DataSet.MoveBy(1);
  {$ifdef dbgDBCntrlGrid}
  DebugLn('%s.DoSelectNext DoSelectNext', [ClassName]);
  {$endif}
  {$IFDEF CONSOLEDEBUG}
  WriteLn('DoSelectNext');
  {$ENDIF}
end;

procedure TDBCntrlGrid.DrawAllRows;
var
  CurActiveRecord: Integer;
begin
  if FDataLink.Active then begin
    {$ifdef dbgGridPaint}
    DebugLnEnter('%s DrawAllRows INIT Link.ActiveRecord=%d, Row=%d',[Name, FDataLink.ActiveRecord, Row]);
    {$endif}
    CurActiveRecord:=FDataLink.ActiveRecord;
    FDrawingEmptyDataset:=FDatalink.DataSet.IsEmpty;
  end else
    FDrawingEmptyDataset:=True;
  try
    inherited DrawAllRows;
  finally
    if FDataLink.Active then begin
      FDataLink.ActiveRecord:=CurActiveRecord;
      {$ifdef dbgGridPaint}
      DebugLnExit('%s DrawAllRows DONE Link.ActiveRecord=%d, Row=%d',[Name, FDataLink.ActiveRecord, Row]);
      {$endif}
    end;
  end;
end;

procedure TDBCntrlGrid.DrawRow(ARow: Integer);
var GridClipRect , ClipArea: TRect;
begin
  //if (ARow>=FixedRows) and FDataLink.Active then
  //  FDrawingActiveRecord := (ARow = FDrawRow)
  //else
  //  FDrawingActiveRecord := False;
  //{$ifdef dbgDBCntrlGrid}
  //DbgOut('DrawRow Row=', IntToStr(ARow), ' Act=', dbgs(FDrawingActiveRecord));
  //{$endif}
  //inherited DrawRow(ARow);
  //{$ifdef dbgDBCntrlGrid}
  //DebugLn(' End Row')
  //{$endif}


  if (ARow>=FixedRows) and FDataLink.Active then begin
    //if (Arow>=FixedRows) and FCanBrowse then
    FDataLink.ActiveRecord:=ARow-FixedRows;
    FDrawingActiveRecord := ARow = Row;
  end else begin
    FDrawingActiveRecord := False;
  end;
  {$ifdef dbgGridPaint}
  DbgOut('DrawRow Row=', IntToStr(ARow), ' Act=', dbgs(FDrawingActiveRecord));
  {$endif}
  inherited DrawRow(ARow);
  {$ifdef dbgGridPaint}
  DebugLn('End Row')
  {$endif}


end;

procedure TDBCntrlGrid.DrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);

function GetDatasetState: TDataSetState;
begin
  if ValidDataSet then
    result := FDataLink.DataSet.State
  else
    result := dsInactive;
end;

begin
  PrepareCanvas(aCol, aRow, aState);
  {$IFDEF CONSOLEDEBUG}
  WriteLn('Draw cell comes, acol=,arow=',acol,',',arow);
  {$ENDIF}

  {$ifdef dbgDBCntrlGrid}
  DebugLn(' ',IntToStr(aCol));
  if gdSelected in aState then DbgOut('S');
  if gdFocused in aState then DbgOut('*');
  if gdFixed in aState then DbgOut('F');
  {$endif dbgGridPaint}

  if aCol < FixedCols then  begin
    {$IFDEF CONSOLEDEBUG}
    WriteLn('DrawIndicator');
    {$ENDIF}
     DrawIndicator(Canvas,aRow, aRect,GetDataSetState,false);
    end
  else
  if (FDrawPanel = nil) or not FDataLink.Active then begin
    {$IFDEF CONSOLEDEBUG}
    WriteLn('DrawFillRect');
    {$ENDIF}
    DrawFillRect(Canvas,aRect);
    end
  else
  if not FDrawingActiveRecord and FDataLink.Active then begin
    DoDrawRow(aRow,aRect,aState);
    end
  else begin
    {$IFDEF CONSOLEDEBUG}
    WriteLn('FDrawingActiveRecord');
    {$ENDIF}
    Canvas.Brush.Color := fSelectedRowIndicatorColor;
    //DrawFillRect(Canvas,aRect);
    end;
  {if we are drawing the active record then this is rendered by the Draw Panel
   i.e. a child control - so we need do nothing here}

  DrawCellGrid(aCol, aRow, aRect, aState);
end;

procedure TDBCntrlGrid.DrawIndicator(ACanvas: TCanvas; aRow: integer;
  R: TRect; Opt: TDataSetState; MultiSel: boolean);
var
  dx,dy, x, y: Integer;
  procedure CenterY;
  begin
    y := R.Top + (R.Bottom-R.Top) div 2;
  end;
  procedure CenterX;
  begin
    X := R.Left + (R.Right-R.Left) div 2;
  end;
  procedure DrawEdit(clr: Tcolor);
  begin
    ACanvas.Pen.Color := clr;
    CenterY;
    CenterX;
    ACanvas.MoveTo(X-2, Y-Dy);
    ACanvas.LineTo(X+3, Y-Dy);
    ACanvas.MoveTo(X, Y-Dy);
    ACanvas.LineTo(X, Y+Dy);
    ACanvas.MoveTo(X-2, Y+Dy);
    ACanvas.LineTo(X+3, Y+Dy);
  end;
  procedure DrawBrowse;
  begin
    ACanvas.Brush.Color:=clBlack;
    ACanvas.Pen.Color:=clBlack;
    CenterY;
    x:= R.Left+3;
    if MultiSel then begin
      if BiDiMode = bdRightToLeft then begin
        ACanvas.Polyline([point(x+dx,y-dy),  point(x,y),point(x+dx,y+dy), point(x+dx,y+dy-1)]);
        ACanvas.Polyline([point(x+dx,y-dy+1),  point(x+1,y),point(x+dx,y+dy-1), point(x+dx,y+dy-2)]);
        CenterX;
        Dec(X,3);
        ACanvas.Ellipse(Rect(X+dx-2,Y-2,X+dx+2,Y+2));
      end else begin
        ACanvas.Polyline([point(x,y-dy),  point(x+dx,y),point(x,y+dy), point(x,y+dy-1)]);
        ACanvas.Polyline([point(x,y-dy+1),point(x+dx-1,y),point(x, y+dy-1), point(x,y+dy-2)]);
        CenterX;
        Dec(X,3);
        ACanvas.Ellipse(Rect(X-2,Y-2,X+2,Y+2));
      end;
    end else begin
      if BiDiMode = bdRightToLeft then
        ACanvas.Polygon([point(x,y),point(x+dx,y-dy),point(x+dx, y+dy),point(x,y)])
      else
        ACanvas.Polygon([point(x,y-dy),point(x+dx,y),point(x, y+dy),point(x,y-dy)]);
    end;
  end;

begin
  if FDrawingActiveRecord then
    ACanvas.Brush.Color := fSelectedRowIndicatorColor
  else
    ACanvas.Brush.Color := FixedColor;
  ACanvas.FillRect(R);
  if aRow <> Row then Exit;

  dx := 6;
  dy := 6;
  case Opt of
    dsBrowse:
      DrawBrowse;
    dsEdit:
      if FDrawingActiveRecord then
        DrawEdit(clBlack)
      else
        DrawBrowse;
    dsInsert:
      if FDrawingActiveRecord then
        DrawEdit(clGreen)
      else
        DrawBrowse;
    else
    if MultiSel then begin
      ACanvas.Brush.Color:=clBlack;
      ACanvas.Pen.Color:=clBlack;
      CenterX;
      CenterY;
      ACanvas.Ellipse(Rect(X-3,Y-3,X+3,Y+3));
    end;
  end;
end;

procedure TDBCntrlGrid.GridMouseWheel(shift: TShiftState; Delta: Integer);
begin
  inherited GridMouseWheel(shift, Delta);
  self.SetFocus;
  if ValidDataSet then begin
    FDataLink.DataSet.MoveBy(Delta);
    {$ifdef dbgDBCntrlGrid}
    DebugLn('GridMouseWheel Delta=', IntToStr(Delta));
    {$endif}
  end;
end;

procedure TDBCntrlGrid.KeyDown(var Key: Word; Shift: TShiftState);
type
  TOperation=(opMoveBy,opCancel,opAppend,opInsert,opDelete);

  procedure DoOnKeyDown;
  begin
    if Assigned(OnKeyDown) then
      OnKeyDown(Self, Key, Shift);
  end;

 procedure DoOperation(AOper: TOperation; Arg: Integer = 0);
  begin
    self.SetFocus;
    case AOper of
      opMoveBy:
        FDatalink.DataSet.MoveBy(Arg);
      opCancel:
        begin
          FDatalink.Dataset.Cancel;
        end;
      opAppend:
        FDatalink.Dataset.Append;
      opInsert:
        FDatalink.Dataset.Insert;
      opDelete:
        FDatalink.Dataset.Delete;
    end;
  end;

  function doVKDown: boolean;
  begin
    if InsertCancelable then
    begin
      if IsEOF then
        result:=true
      else begin
        doOperation(opCancel);
        result := false;
      end;
    end else begin
      result:=false;
      doOperation(opMoveBy, 1);
      if GridCanModify and FDataLink.EOF then begin
        if not (dgpDisableInsert in Options) then
          doOperation(opAppend);
      end
    end;
  end;

  function DoVKUP: boolean;
  begin
    if InsertCancelable then
      doOperation(opCancel)
    else begin
      doOperation(opMoveBy, -1);
    end;
    result := FDatalink.DataSet.BOF;
  end;



begin
  case Key of
    VK_DOWN:
      begin
        DoOnKeyDown;
        if (Key<>0) and ValidDataset then begin
          doVKDown;
          Key := 0;
        end;
      end;

    VK_UP:
      begin
        doOnKeyDown;
        if (Key<>0) and ValidDataset then begin
          doVKUp;
          key := 0;
         end;
      end;

    VK_NEXT:
      begin
        doOnKeyDown;
        if (Key<>0) and ValidDataset then begin
          doOperation(opMoveBy, VisibleRowCount);
          Key := 0;
        end;
      end;

    VK_PRIOR:
      begin
        doOnKeyDown;
        if (Key<>0) and ValidDataset then begin
          doOperation(opMoveBy, -VisibleRowCount);
          key := 0;
        end;
      end;

    VK_ESCAPE:
      begin
        doOnKeyDown;
        if ValidDataSet then
           doOperation(opCancel);
      end;

    VK_HOME:
      begin
        doOnKeyDown;
        if (Key<>0) and ValidDataSet then
        begin
            if ssCTRL in Shift then
            begin
              FDataLink.DataSet.First;
              Key:=0;
            end;
        end;
      end;

    VK_END:
      begin
        doOnKeyDown;
        if Key<>0 then begin
          if ValidDataSet then
          begin
            if ssCTRL in shift then
            begin
              FDatalink.DataSet.Last;
              Key:=0;
            end;
          end;
        end;
      end;

  end;
end;

procedure TDBCntrlGrid.LinkActive(Value: Boolean);
var
  B1 : TBookMark;
begin
  {$ifdef dbgDBCntrlGrid}
  DebugLn('%s.LinkActive Value=%s', [ClassName, Value.ToString(True)]);
  {$endif}
  if not Value then
  begin
    FRowCache.ClearCache;
    Row := FixedRows;
    FDrawingActiveRecord := false;
    FSelectedRecNo := 0;
    FSelectedRow := 0;
    FRequiredRecNo := 0;
  end;
  FRowCache.UseAlternateColors := AlternateColor <> Color;
  FRowCache.AltColorStartNormal := AltColorStartNormal;
  FLastRecordCount := 0;
  LayoutChanged;
  if Value then
  begin
    { The problem being solved here is that TDataSet does not readily tell us
      when a record is deleted. We get a DataSetChanged event - but this can
      occur for many reasons. Only by monitoring the record count accurately
      can be determine when a record is deleted. To do this we need to scroll
      the whole dataset to the end when the dataset is activated. Not desirable
      with large datasets - but a fix to TDataSet is needed to avoid this.
    }
    FDataLink.DataSet.DisableControls;
    try
      FRequiredRecNo := FDataLink.DataSet.RecNo;
      B1 := FDataLink.DataSet.GetBookmark;
      FDataLink.DataSet.Last;
      FLastRecordCount := FDataLink.DataSet.RecordCount;
      if not FDefaultPositionAtEnd then begin
        FDataLink.DataSet.GotoBookmark( B1 );
        end;
      FDataLink.DataSet.FreeBookmark( B1 );
        //FDataLink.DataSet.First;

    finally
      FDataLink.DataSet.EnableControls;
    end;
  end;
end;

procedure TDBCntrlGrid.LayoutChanged;
var
  Cnt: Integer;
  B1 : TBookMark;
begin
  if csDestroying in ComponentState then
    exit;
  {$ifdef dbgDBCntrlGrid}
  DebugLnEnter('%s.LayoutChanged INIT', [ClassName]);
  {$endif}
  BeginUpdate;
  try
    Cnt := UpdateGridCounts;
    if Cnt=0 then
      EmptyGrid;
  finally
     EndUpdate;
    {$ifdef dbgDBCntrlGrid}
    DebugLnExit('%s.LayoutChanged DONE', [ClassName]);
    {$endif}
  end;
  UpdateScrollbarRange;
end;

procedure TDBCntrlGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Gz: TGridZone;
  P: TPoint;
  procedure doMouseDown;
  begin
//    if not Focused then
//      SetFocus;
    if assigned(OnMouseDown) then
      OnMouseDown(Self, Button, Shift, X, Y);
  end;
  procedure doInherited;
  begin
    inherited MouseDown(Button, Shift, X, Y);
  end;
  procedure doMoveBy;
  begin
    FDatalink.DataSet.MoveBy(P.Y - Row);
  end;
  procedure doMoveToColumn;
  begin
    Col := P.X;
  end;
  procedure DoCancel;
  begin
    FDatalink.Dataset.cancel;
  end;
begin
  if (csDesigning in componentState) or not ValidDataSet then begin
    exit;
  end;
  self.SetFocus;

{  if not MouseButtonAllowed(Button) then begin
    doInherited;
    exit;
  end;}
  {$ifdef dbgDBCntrlGrid}DebugLnEnter('%s.MouseDown INIT', [ClassName]); {$endif}
  Gz:=MouseToGridZone(X,Y);
  CacheMouseDown(X,Y);
  case Gz of
    gzInvalid:
      doMouseDown;

    gzFixedCells, gzFixedCols:
      doInherited;
    else
      begin

        P:=MouseToCell(Point(X,Y));
        if Gz=gzFixedRows then
          P.X := Col;

        if P.Y=Row then begin
          //doAcceptValue;

          if not (ssCtrl in Shift) then
          begin
            if gz=gzFixedRows then
              doMouseDown
            else
              doInherited;
          end;

        end else begin
          doMouseDown;
          if ValidDataSet then begin
            if InsertCancelable and IsEOF then
              doCancel;
            doMoveBy;
          end;
        end;
      end;
  end;
  {$ifdef dbgDBCntrlGrid}DebugLnExit('%s.MouseDown DONE', [ClassName]); {$endif}
end;

procedure TDBCntrlGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  {$ifdef dbgDBCntrlGrid}DebugLnEnter('%s.MouseUp INIT', [ClassName]); {$endif}
  FLastMouse.X := X;
  FLastMouse.Y := Y;
  FLastMouseButton := Button;
  FLastMouseShiftState := Shift;
  Application.QueueAsyncCall(@DoSendMouseClicks,0);
  //DoSendMouseClicks( 0 );
  {$ifdef dbgDBCntrlGrid}DebugLnExit('%s.MouseUp DONE', [ClassName]); {$endif}
end;

procedure TDBCntrlGrid.MoveSelection;
begin
  inherited MoveSelection;
  UpdateActive;
  InvalidateRow(Row);
end;

procedure TDBCntrlGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and
     (AComponent = FDrawPanel) then FDrawPanel := nil;
end;

procedure TDBCntrlGrid.PrepareCanvas(aCol, aRow: Integer; aState: TGridDrawState
  );
begin
  inherited PrepareCanvas(aCol, aRow, aState);

  if gdFixed in aState then
  begin
    if gdHot in aState then
      Canvas.Brush.Color := FixedHotColor
    else
      Canvas.Brush.Color := GetColumnColor(aCol, gdFixed in AState);
  end;

  if (not FDatalink.Active) and ((gdSelected in aState) or (gdFocused in aState)) then
    Canvas.Brush.Color := Self.Color;

end;

procedure TDBCntrlGrid.ResetSizes;
begin
  LayoutChanged;
  inherited ResetSizes;
end;

procedure TDBCntrlGrid.SetColor(Value: TColor);
begin
  inherited SetColor(Value);
  if (csDesigning in ComponentState) and assigned(FDrawPaneL) then
     FDrawPanel.Color := Value;
end;

procedure TDBCntrlGrid.UpdateActive;
//var
//  PrevRow: Integer;
//  Rn: LongInt;
//begin
//  if (csDestroying in ComponentState) or
//    (FDatalink=nil) or (not FDatalink.Active) or
//    (FDatalink.ActiveRecord<0) then
//    exit;
//  {$ifdef dbgDBCntrlGrid}
//  DebugLn('%s.UpdateActive (%s): ActiveRecord=%d FixedRows=%d Row=%d',
//  		[ClassName, Name, FDataLink.ActiveRecord, FixedRows, Row]);
//  {$endif}
//  if Assigned( OnUpdateActive ) then
//    OnUpdateActive( FDataLink );
//  Rn := FDataLink.DataSet.RecNo;
//
//  FDrawRow := FixedRows + FDataLink.ActiveRecord;
//  FSelectedRecNo := FDataLink.DataSet.RecNo;
//
//  PrevRow := Row;
//  Row := FDrawRow;
//  if not FInCacheRefresh then
//  begin
//    FSelectedRow := FDrawRow;
//    if FDatalink.DataSet.State <> dsInsert then
//      FRowCache.InvalidateRowImage(FSelectedRecNo);
//  end;
//  InvalidateRow(PrevRow);
//  SetupDrawPanel(FDrawRow);



var
  PrevRow: Integer;
  NewRow: Integer;
begin
  if (csDestroying in ComponentState) or
    (FDatalink=nil) or (not FDatalink.Active) or
    (FDatalink.ActiveRecord<0) then
    exit;
  {$ifdef dbgDBGrid}
  DebugLn('%s.UpdateActive (%s): ActiveRecord=%d FixedRows=%d Row=%d',
    	      [ClassName, Name, FDataLink.ActiveRecord, FixedRows, Row]);
  {$endif}
  if Assigned( OnUpdateActive ) then
    OnUpdateActive( FDataLink );
  PrevRow := Row;
  NewRow:= FixedRows + FDataLink.ActiveRecord;
  if NewRow>RowCount-1 then
    NewRow := RowCount-1;
  Row := NewRow;
  fDrawRow := Row;
  FSelectedRecNo := FDataLink.DataSet.RecNo;
  FSelectedRow := fDrawRow;
  if PrevRow<>Row then
    InvalidateRow(PrevRow);
  //if FDatalink.DataSet.State <> dsInsert then
  //  FRowCache.InvalidateRowImage(FSelectedRecNo);
  InvalidateRow(PrevRow);
  SetupDrawPanel( fDrawRow );

end;

procedure TDBCntrlGrid.UpdateData;
begin
  {$ifdef dbgDBCntrlGrid}
  DebugLn('%s.UpdateData', [ClassName]);
  {$endif}
  FModified := false;
end;

procedure TDBCntrlGrid.UpdateShowing;
begin
  {$ifdef dbgDBCntrlGrid}
  DebugLn('%s.UpdateShowing', [ClassName]);
  {$endif}
  inherited UpdateShowing;
end;

procedure TDBCntrlGrid.UpdateVertScrollbar(const aVisible: boolean;
  const aRange, aPage, aPos: Integer);
begin
  {$ifdef dbgDBCntrlGrid}
  DebugLn('%s.UpdateVertScrollbar aRange=%d aPage=%d aPos=%d', [ClassName,aRange, aPage, aPos]);
  {$endif}
  UpdateScrollbarRange;
end;

procedure TDBCntrlGrid.DoOnChangeBounds;
begin
  BeginUpdate;
  inherited DoOnChangeBounds;
  if HandleAllocated then
    LayoutChanged;
  EndUpdate;
end;

constructor TDBCntrlGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TDBCntrlGridDataLink.Create;//(Self);
  FRowCache := TRowCache.Create;
  FDataLink.OnRecordChanged:=@OnRecordChanged;
  FDataLink.OnDatasetChanged:=@OnDataSetChanged;
  FDataLink.OnDataSetOpen:=@OnDataSetOpen;
  FDataLink.OnDataSetClose:=@OnDataSetClose;
  FDataLink.OnNewDataSet:=@OnNewDataSet;
  FDataLink.OnInvalidDataSet:=@OnInvalidDataset;
  FDataLink.OnInvalidDataSource:=@OnInvalidDataSource;
  FDataLink.OnDataSetScrolled:=@OnDataSetScrolled;
  FDataLink.OnLayoutChanged:=@OnLayoutChanged;
  FDataLink.OnEditingChanged:=@OnEditingChanged;
  FDataLink.OnUpdateData:=@OnUpdateData;
  FDataLink.OnCheckBrowseMode := @OnCheckBrowseMode;
  FDataLink.VisualControl:= True;
  fResizetimer := TTimer.Create(nil);
  fResizetimer.Interval := 10;
  fResizetimer.OnTimer := @ResizeTitemTimer;
  fResizetimer.Enabled := False;
  fSaveAfterScrollHandler := nil;
  ScrollBars := ssAutoVertical;
  FOptions := [dgpIndicator];
  FixedCols := 1;
  ColCount := 1;
  FixedRows := 0;
  RowCount := 1;
  fSelectedRowIndicatorColor:=clBtnFace;
  ColWidths[0] := Scale96ToFont(COL_WIDTH);
  Columns.Add.ReadOnly := true; {Add Dummy Column for Panel}
  DoGridResize;
  if not (csDesigning in ComponentState) then
    Application.AddOnKeyDownBeforeHandler(@KeyDownHandler,false);
end;

destructor TDBCntrlGrid.Destroy;
begin
  fResizetimer.Enabled:=False;
  fResizetimer.Free;
  if assigned(FDataLink) then
  begin
    FDataLink.OnDataSetChanged:=nil;
    FDataLink.OnRecordChanged:=nil;
    FDataLink.Free;
  end;
  if assigned(FRowCache) then FRowCache.Free;
  Application.RemoveAsyncCalls(self);
  if not (csDesigning in ComponentState) then
    Application.RemoveOnKeyDownBeforeHandler( @KeyDownHandler );
  inherited Destroy;
end;


function TDBCntrlGrid.MouseToRecordOffset(const x, y: Integer;
  out RecordOffset: Integer): TGridZone;
var
  aCol,aRow: Integer;
begin
  Result := MouseToGridZone(x, y);

  RecordOffset := 0;

  if (Result=gzInvalid) or (Result=gzFixedCells) then
    exit;

  MouseToCell(x, y, aCol, aRow);

  if (Result=gzFixedRows) or (Result=gzNormal) then
    RecordOffset := aRow - Row;

  if (Result=gzFixedCols) or (Result=gzNormal) then begin
    aRow := ColumnIndexFromGridColumn(aCol);
  end;
end;

function TDBCntrlGrid.ExecuteAction(AAction: TBasicAction): Boolean;
begin
  Result := (DataLink <> nil)
            and DataLink.ExecuteAction(AAction);
end;

function TDBCntrlGrid.UpdateAction(AAction: TBasicAction): Boolean;
begin
  Result := (DataLink <> nil)
            and DataLink.UpdateAction(AAction);
end;
//
// New method for refresh all missing bitmaps.
// Stupid, but only this is a way right now;
//
procedure TDBCntrlGrid.RefreshImagesCycle;
var
  ff: TRowDetails;
  fRecNo , i: integer;
  B1 : TBookMark;
begin
  if not ValidDataSet then Exit;
  BeginUpdate;
  fSaveAfterScrollHandler := FDataLink.DataSet.AfterScroll;
  //FDataLink.DataSet.AfterScroll := nil;
  B1 := FDataLink.DataSet.GetBookmark;
  try
  fRecNo := 0;
  if Length( FRowCache.FList ) > 0 then begin
    FDataLink.DataSet.Locate(KeyField, FRowCache.FList[ 0 ].aRecKey, [] );
    for i := 1 to Length( FRowCache.FList ) -1   do begin
      FDataLink.DataSet.Next;
      end;
    end;

  finally
  FDataLink.DataSet.AfterScroll := fSaveAfterScrollHandler;
  FDataLink.DataSet.GotoBookmark( B1 );
  FDataLink.DataSet.FreeBookmark( B1 );
  EndUpdate();
  end;
end;


procedure Register;
begin
  {$I dbcontrolgrid_icon.lrs}
  RegisterComponents('Data Controls',[TDBCntrlGrid]);
end;
initialization
{$IFDEF CONSOLEDEBUG}
  {$IFDEF WINDOWS}
  AllocConsole;      // in Windows unit
  IsConsole := True; // in System unit
  SysInitStdIO;      // in System unit
  {$ENDIF}
{$ENDIF}
end.

