unit UIunit;

interface
//{$DEFINE uidebug}
uses
  gameSpriteUnit, System.UITypes, System.SysUtils, System.Classes, FMX.Graphics,
  System.Generics.Collections, System.Types, FMX.Types, FMX.Edit, TeeGenericTree;

const
  UIDefaultFontSize = 24.0;
  UIDefaultTextColor = TAlphaColorRec.Silver;
  UIDefaultItemNumColor = TAlphaColorRec.White;
  UIDefaultBgColor = $80000000;
  UIDefaultFontStyle = [];
  UIListItemDefFontSize = 18.0;
  DefPnlBgImgFile = 'interface\蓝色风格\方框\深色底\';
  DefPnlClsBtnImgFile = 'interface\蓝色风格\按钮\关闭.png';
  DefBtnImgFiles: array[0..1] of string = (
    'interface\button\button0.png', 'interface\button\button1.png');
  DefChkImgFile = 'interface\蓝色风格\按钮\圆勾选.png';
type
  TUIStrechMode = (smNone, smCenter, smFit, smLeft, smFull, sm3Slot, smV3Slot, sm9Slot);
  TUIPosition = (upCenter, upTopCenter, upBottomCenter, upTopLeft, upTopRight,
    upBottomLeft, upBottomRight, upLeftCenter, upRightCenter);
  TUITextPosition = (tpCenter, tpBmpRight);
  TUIElement = class(TFmxObject)
  public class var
    FUIOwner: TComponent;
    FCanvas: TCanvas;
    FActiveScene: TUIElement;
    FDownElement, FOverElement, FEditElement: TUIElement;
    FDownPoint: TPointF;
    FDownElementPosition: TPoint;
    FDownTime, FUpTime: Int64;
    FFmxEdit: TEdit;
    FCheckTimer: TTimer;
  private
    FTop, FLeft, FWidth, FHeight: Integer;
    FOpacity: Single;
    FFontSize: Single;
    FFontStyle: TFontStyles;
    FFontColor: TAlphaColor;
    FTextAlign, FVTextAlign: TTextAlign;
    FTextPosition: TUITextPosition;
    FOnMouseDown, FOnMouseUp: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnDblClick: TNotifyEvent;

    procedure setOpacity(const Value: Single);
    procedure SetX(const Value: integer);
    function GetParent: TUIElement;
    procedure SetParent(const Value: TUIElement);
    procedure SetHeight(const Value: Integer);
    procedure SetLeft(const Value: Integer);
    procedure SetTop(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    function GetVisible: Boolean;
    function getBoundsF: TRectF;
    procedure setStretchMode(const Value: TUIStrechMode);
    function Get(idx: Integer): TUIElement;
    procedure setY(const Value: Integer);
    class procedure setFmxEdit(const Value: TEdit); static;
  public
    FText: string;
    FX, FY: Integer;
    FTag: Integer;
    FScale: TPointF;
    FEnabled, FVisible, FCanHit, FCanMove: Boolean;
    FSprBmps: array[0..8] of TGameSprBmp;
    FFrameIndex, FFrameCount: Integer;
    FStretchMode: TUIStrechMode;

//    FChildList: TList<TUIElement>;
    FDown: Boolean;

    constructor Create(name: string; prnt: TUIElement); overload; virtual;
    constructor Create(name: string; prnt: TUIElement; l, t, w, h: Integer); overload; virtual;
    constructor Create(name: string; prnt: TUIElement; txt: string; l, t, w, h: Integer); overload; virtual;
    destructor Destroy; override;
    procedure setSprBmp(idx: Integer; sprBmp: TGameSprBmp);
    procedure addChild(child: TUIElement);
    procedure setPostion(left, top: Integer); overload;
    procedure setPostion(p: TUIPosition); overload;
    procedure move(left, top: Integer);
    procedure setSize(w, h: Integer);
    procedure show;
    procedure hide; virtual;


    procedure draw(frmIdx: Integer = -1); virtual;
    function mouseDown(btn: TMouseButton;sft: TShiftState; p: TPointF): Boolean; virtual;
    function mouseUp(btn: TMouseButton; sft: TShiftState; p: TPointF): Boolean; virtual;
    function mouseMove(sft: TShiftState; p: TPointF): Boolean; virtual;
    procedure dblClick;

    function getPosition: TPoint;
    function getClientRect: TRectF;

    property Top: Integer read FTop write SetTop;
    property Left: Integer read FLeft write SetLeft;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property Opacity: Single read FOpacity write setOpacity;
    property X: Integer read FX write setX;
    property Y: Integer read FY write setY;
    property StretchMode: TUIStrechMode read FStretchMode write setStretchMode;
    property FontSize: Single read FFontSize write FFontSize;
    property FontStyle: TFontStyles read FFontStyle write FFontStyle;
    property FontColor: TAlphaColor read FFontColor write FFontColor;
    property TextAlign: TTextAlign read FTextAlign write FTextAlign;
    property VTextAlign: TTextAlign read FVTextAlign write FVTextAlign;
    property TextPosition: TUITextPosition read FTextPosition write FTextPosition;

    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;

    property Parent: TUIElement read GetParent write SetParent;
    property Visible: Boolean read GetVisible write FVisible;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Text: string read FText write FText;
    property BoundsF: TRectF read getBoundsF;
    property ChildList[idx: Integer]: TUIElement read Get; default;

    class property FmxEdit: TEdit read FFmxEdit write setFmxEdit;
    class function uiMouseDown(Button: TMouseButton;
      Shift: TShiftState; X, Y: Single): Boolean;
    class function uiMouseUp(Button: TMouseButton;
      Shift: TShiftState; X, Y: Single): Boolean;
    class function uiMouseMove(Shift: TShiftState; X, Y: Single): Boolean;
    class procedure uiCheckLongTap;
    class procedure uiEditChange(sender: TObject);
    class procedure uiEditKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    class procedure drawGameUI;
    class function findUI(sname: string): Pointer;
    class procedure FCheckTimerTimer(Sender: TObject);

    procedure loadBmp(fname: string);
    procedure load3slotBmps(spath: string);
    procedure loadV3slotBmps(spath: string);
    procedure load9slotBmps(spath: string);
    procedure loadItemBmp(itmId: Integer; bigBmp: Boolean = False);
    procedure loadObjBmp(objId: Integer);
    procedure clearBmps;
    procedure alignChildrenH;
    procedure alignChildrenV;
  end;

  TUIScene = class(TUIElement)
  public
    constructor Create(name: string; prnt: TUIElement); override;
    function mouseDown(btn: TMouseButton;sft: TShiftState; p: TPointF): Boolean; override;
    function mouseMove(sft: TShiftState; p: TPointF): Boolean; override;
    function mouseUp(btn: TMouseButton; sft: TShiftState; p: TPointF): Boolean; override;
  end;

  TUIButton = class(TUIElement)
  public
    OnClick: TNotifyEvent;

    constructor Create(name: string; prnt: TUIElement; l, t, w, h: Integer; txt: string = '';
      defBtnType: Integer = -1); virtual;
//    procedure draw(frmIdx: Integer = -1); override;
    function mouseUp(btn: TMouseButton; sft: TShiftState; p: TPointF): Boolean; override;
  end;

  TUICheckbox = class(TUIElement)
  private
  public
    constructor Create(name: string; prnt: TUIElement; txt: string; l, t, w, h: Integer); override;
    function mouseUp(btn: TMouseButton; sft: TShiftState; p: TPointF): Boolean;  override;
    procedure draw(frmIdx: Integer = -1); override;
    property Checked: Boolean read FDown write FDown;
  end;

  TUILabel = class(TUIElement)
  public

    constructor Create(name: string; prnt: TUIElement; txt: string; x, y, w, h: Integer; align: TTextAlign = TTextAlign.Center;
      size: Single = UIDefaultFontSize; style: TFontStyles = UIDefaultFontStyle; clr: TAlphaColor = UIDefaultTextColor);
    procedure draw(frmIdx: Integer = -1); override;
  end;

  /// <summary>
  /// base on fmx TEdit, must set Form.edtUI events as following
  /// Form1.edtUI.OnChange := TUIElement.uiEditChange;
  /// Form1.edtUI.OnKeyUp := TUIElement.uiEditKeyUp;
  /// </summary>
  TUIEdit = class(TUIElement)
  private
    FSelStart, FSelLength, FCaretPos: Integer;
    FOnKeyUp: TKeyEvent;
    FOnChange: TNotifyEvent;
    function getValue: Integer;
    procedure setValue(const Value: Integer);
    procedure setText(const Value: string);
    procedure doChange;
  public
    FFontColor: TAlphaColor;
    constructor Create(name: string; prnt: TUIElement; l, t, w, h: Integer; clr: TAlphaColor = UIDefaultTextColor);
    function mouseUp(btn: TMouseButton; sft: TShiftState; p: TPointF): Boolean; override;
    procedure draw(idx: Integer); override;
    property Value: Integer read getValue write setValue;
    property Text: string read FText write setText;
    property OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property SelStart: Integer read FSelStart write FSelStart;
    property SelLength: Integer read FSelLength write FSelLength;
    property CaretPos: Integer read FCaretPos write FCaretPos;
  end;

  TUIAni = class(TUIElement)
  private
    FLastDrawTime: Int64;
    FDuration: Integer;
  public
    constructor Create(name: string; prnt: TUIElement; l, t, w, h: Integer); override;
    procedure draw(idx: Integer); override;
    property Duration: Integer read FDuration write FDuration;
  end;

  TUISpinBox = class(TUIElement)
  private
    FEdit: TUIEdit;
    FBtnUp, FBtnDn: TUIButton;
    FOnChange: TNotifyEvent;

    procedure btnUpClick(sender: TObject);
    procedure btnDnClick(sender: TObject);
    function getValue: Integer;
    procedure setValue(const Value: Integer);
    function getText: string;
  public
    constructor Create(name: string; prnt: TUIElement; txt: string; l, t, w, h: Integer); override;

    procedure ValueInc;
    property Value: Integer read getValue write setValue;
    property Text: string read getText;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TUIRectangle = class(TUIElement)
  public
    FXRadius, FYRadius: Single;
    FColor: TAlphaColor;
    FDrawType: Integer;

    constructor Create(name: string; prnt: TUIElement; l, t, w, h: Integer; clr: TAlphaColor);
    procedure draw(frmIdx: Integer = -1); override;
  end;

  TUIPanel = class(TUIElement)
  private
    FOnClose: TNotifyEvent;
  public
    FBtnClose: TUIButton;

    constructor Create(name: string; prnt: TUIElement; w, h: Integer; closeBtn: Boolean = True;
      txt: string = ''; defBmps: Boolean = True);
    procedure btnCloseClick(sender: TObject);
    procedure loadBmps(btnFile: string = DefPnlClsBtnImgFile; bgFile: string = DefPnlBgImgFile);
    procedure hide; override;

    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;

  TUIGridType = (gtString, gtGameItem, gtInventory);
  TUIIntChangeEvent = procedure (sender: TObject; idx: Integer) of object;
  TUIGrid = class(TUIElement)
  private
    FOnGridClick: TUIIntChangeEvent;
    FOnGridDblClick: TUIIntChangeEvent;
    FMouseDownPoint, FLastMousePoint: TPointF;
    FMouseDownTopIndex: Integer;
    FTopIndex, FGridIndex: Integer;
    FBgColor: TAlphaColor;
    FRowSelect: Boolean;
    FRowHeights, FColWidths: array of Integer;
    FGridType: TUIGridType;

    function getPgSize: Integer;
    procedure setTopIndex(const Value: Integer);
    procedure setTextAlign(const Value: TTextAlign);
    function getSelected: TUIElement;
    function getRowIndex: Integer;
    function getColIndex: Integer;
    procedure resetRectList;
    function GetColWidths(idx: Integer): Integer;
    function GetRowHeights(idx: Integer): Integer;
    procedure SetColWidths(idx: Integer; const Value: Integer);
    procedure SetRowHeights(idx: Integer; const Value: Integer);
  protected
    FRectList: TList<TRectF>;
    function getCount: Integer; virtual; abstract;
  public
    FRowCount, FColCount, FPgSize: Integer;

    FLineColor: TAlphaColor;
    FMovingGrid: TUIElement;

    constructor Create(name: string; prnt: TUIElement; r, c, l, t, w, h: Integer); virtual;
    destructor Destroy; override;
    function mouseDown(btn: TMouseButton;sft: TShiftState; p: TPointF): Boolean; override;
    function mouseMove(sft: TShiftState; p: TPointF): Boolean; override;
    function mouseUp(btn: TMouseButton; sft: TShiftState; p: TPointF): Boolean; override;
    procedure drawGrid(idx: Integer); virtual; abstract;
    procedure draw(frmIdx: Integer = -1); override;

    procedure clear; virtual; abstract;

    property GridType: TUIGridType read FGridType write FGridType;

//    property Selected: TUIElement read getSelected;
    property PgSize: Integer read getPgSize;
    property Count: Integer read getCount;
    property RowHeights[idx: Integer]: Integer read GetRowHeights write SetRowHeights;
    property ColWidths[idx: Integer]: Integer read GetColWidths write SetColWidths;
    property RowIndex: Integer read getRowIndex;
    property ColIndex: Integer read getColIndex;
    property TopIndex: Integer read FTopIndex write setTopIndex;
    property BgColor: TAlphaColor read FBgColor write FBgColor;
    property TextAlign: TTextAlign read FTextAlign write setTextAlign;
    property RowSelect: Boolean read FRowSelect write FRowSelect;
    property OnGridClick: TUIIntChangeEvent read FOnGridClick write FOnGridClick;
    property OnGridDblClick: TUIIntChangeEvent read FOnGridDblClick write FOnGridDblClick;
  end;

  TUIListbox = class(TUIGrid)
  protected
    FItems: TStrings;
    function getCount: Integer; override;
  public
    constructor Create(name: string; prnt: TUIElement; r, c, l, t, w, h: Integer); override;
    destructor Destroy; override;
    procedure drawGrid(idx: Integer); override;
    procedure clear; override;

    property Items: TStrings read FItems;
    property ItemIndex: Integer read FGridIndex write FGridIndex;
    property Count: Integer read getCount;
  end;

  TUIListView = class;
  TUIListItem = class
  private
    FParent: TUIListView;
    FLastDrawTime: Int64;
    FText, FValue: string;
    FFontColor: TAlphaColor;
    FImgSize: Integer;
    FData: Pointer;
    FTag: NativeInt;
    FVisible: Boolean;
  public
    FSprBmps: array[0..8] of TGameSprBmp;
    FFrameIndex, FFrameCount: Integer;

    constructor Create(AParent: TUIListView);

    property Text: string read FText write FText;
    property Value: string read FValue write FValue;
    property FontColor: TAlphaColor read FFontColor write FFontColor;
    property LastDrawTime: Int64 read FLastDrawTime write FLastDrawTime;
    property ImgSize: Integer read FImgSize write FImgSize;
    property Data: Pointer read FData write FData;
    property Tag: NativeInt read FTag write FTag;
    property Visible: Boolean read FVisible write FVisible;
    property Parent: TUIListView read FParent;

    procedure loadBmp(fname: string);
    procedure loadItemBmp(itmId: Integer; bigBmp: Boolean = False);
    procedure loadObjBmp(objId: Integer);
  end;
  TUIListViewDrawItemEvent = procedure (AItem: TUIListItem; ARect: TRectF;
    ACanvas: TCanvas; var DrawOver: Boolean) of object;
  TUIListView = class(TUIGrid)
  private
    FDuration: Integer;
    FOnDrawItemText, FOnDrawItemImage: TUIListViewDrawItemEvent;
    function getFocusedItem: TUIListItem;
  protected
    FItems: TObjectList<TUIListItem>;
    function getCount: Integer; override;
  public
    constructor Create(name: string; prnt: TUIElement; r, c, l, t, w, h: Integer); override;
    destructor Destroy; override;
    procedure drawGrid(idx: Integer); override;
    function Add: TUIListItem;
    procedure clear; override;
    procedure hideAll;

    property Items: TObjectList<TUIListItem> read FItems;
    property ItemIndex: Integer read FGridIndex write FGridIndex;
    property Duration: Integer read FDuration write FDuration;
    property Count: Integer read getCount;
    property OnDrawItemImage: TUIListViewDrawItemEvent read FOnDrawItemImage write FOnDrawItemImage;
    property OnDrawItemText: TUIListViewDrawItemEvent read FOnDrawItemText write FOnDrawItemText;
    property FocusedItem: TUIListItem read getFocusedItem;
  end;

  TUIComboBox = class(TUIButton)
  private
    FItemList: TUIListbox;
    procedure OnItemClick(sender: TObject; idx: Integer);
    procedure OnSelfClick(sender: TObject);
    procedure setItemIndex(const Value: Integer);
    function getItems: TStrings;
    function getItemIndex: Integer;
  public
    constructor Create(name: string; prnt: TUIElement; l, t, w, h: Integer; txt: string = '';
      defBtnType: Integer = -1); override;

    property Items: TStrings read getItems;
    property ItemIndex: Integer read getItemIndex write setItemIndex;

  end;

  TUIRichView = class;

  TRVBlockType = (rbtText, rbtImage);
  TRVAction = (raNone, raClick);
  TRVBlock = class
  public
    prnt: TUIRichView;
    blockType: TRVBlockType;
    left, right: Single;
    script: string;

    constructor Create(rdt: TUIRichView);
    procedure draw(y, h: Single); virtual; abstract;
  end;
  TRVTextBlock = class(TRVBlock)
  public
    text: string;
    size: Single;
    color: TAlphaColor;
    style: TFontStyles;

    procedure draw(y, h: Single); override;
  end;

  TRVLine = class
  private
    FHeight, FWidth: Single;
    FParent: TUIRichView;
  public
    blockList: TList<TRVBlock>;

    property Height: Single read FHeight;
    property Width: Single read FWidth;
    property Parent: TUIRichView read FParent;

    constructor Create(redt: TUIRichView);
    destructor Destroy; override;

    function addTextBlock: TRVTextBlock;
    function getBlock(x: Single): TRVBlock;

    procedure draw(y: Single);
  end;

  TUIScrollBar = class(TUIElement)
  public
    Value, Max: Single;
  end;

  TRVActionEvent = procedure (blk: TRVBlock) of object;
  TUIRichView = class(TUIElement)
  private
//    FCanvas: TCanvas;
    FVScrollBar, FHScrollBar: TUIScrollBar;
    FFont: TFont;
    FTextColor, FBgColor: TAlphaColor;
    FFontChanged: Boolean;
    FBounds: TRectF;
//    FMouseDown: Boolean;
    FMouseDownPoint, FLastMousePoint, FMouseDonwSBValue: TPointF;
    FMouseDownLineIndex: Integer;

    FOnAction: TRVActionEvent;
    FActionBlk: TRVBlock;

    FLines: TList<TRVLine>;
    FOriginalLines: TStringList;
    oldLineIndex: Integer;
    updating: Boolean;

    function mouseDown(btn: TMouseButton;sft: TShiftState; p: TPointF): Boolean; override;
    function mouseMove(sft: TShiftState; p: TPointF): Boolean; override;
    function mouseUp(btn: TMouseButton; sft: TShiftState; p: TPointF): Boolean; override;
    procedure vscrollBarChange(Sender: TObject);
    procedure hscrollBarChange(Sender: TObject);
  public
    tag, topLineIndex, bottomLineIndex, lineIndex: Integer;
    defFontSize: Single;
    defTextColor: TAlphaColor;
    defFontStyle: TFontStyles;
    wordWrap: Boolean;
    autoScroll: Boolean;

    constructor Create(name: string; prnt: TUIElement; l, t, w, h: Integer;
      fclr: TAlphaColor = UIDefaultTextColor; bgClr: TAlphaColor = UIDefaultBgColor);
    destructor Destroy; override;

    procedure addText(s: string; scr: string = '');
    function addNewLine: TRVLine;
    procedure delete(idx: Integer);

    procedure setFont(fclr: TAlphaColor; fsize: Single = 0; fstyle: TFontStyles = []);
    procedure resetFont;

    procedure beginUpdate;
    procedure endUpdate;
    procedure draw(frmIdx: Integer = -1); override;
    procedure clear;

    property Lines: TList<TRVLine> read FLines;
//    property Width: Single read FWidth;
//    property Height: Single read FHeight;
    property TextColor: TAlphaColor read FTextColor;
    property BgColor: TAlphaColor read FBgColor write FBgColor;

    property OnAction: TRVActionEvent read FOnAction write FOnAction;

  end;

  TUITreeViewItem = class
  public
    FRect: TRectF;
    FData: string;
    FExpanded, FSelected, FVisible: Boolean;
    FTag: Integer;

    constructor Create(txt: string; w, h: Single);
    property Expanded: Boolean read FExpanded write FExpanded;
    property Visible: Boolean read FVisible write FVisible;
    property Tag: Integer read FTag write FTag;
  end;
  TUITreeViewNode = TNode<TUITreeViewItem>;

  TUITreeView = class(TUIElement)
  private
    FItems: TUITreeViewNode;
    FBtnBmp: TGameSprBmp;
    FItemSize: TSizeF;
    FTVDownPt: TPointF;

    FOnClickItem: TNotifyEvent;
    procedure tvMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);

  public
    constructor Create(name: string; prnt: TUIElement; l, t, w, h: Integer); override;
    destructor Destroy; override;

    procedure draw(idx: Integer); override;
    function mouseUp(btn: TMouseButton; sft: TShiftState; p: TPointF): Boolean; override;
    function add(txt: string): TUITreeViewNode;
    function addChild(node: TUITreeViewNode; txt: string): TUITreeViewNode;
    property Items: TUITreeViewNode read FItems;
    property OnClickItem: TNotifyEvent read FOnClickItem write FOnClickItem;
  end;

var
  gameUI: TUIScene;

implementation

uses
  System.Math, pubUnit{$ifdef gameclient}, iniUnit{$endif}, System.Math.Vectors;

{ TUIElement }

procedure TUIElement.addChild(child: TUIElement);
begin
//  if not Assigned(FChildList) then
//    FChildList := TList<TUIElement>.Create
//  else if FChildList.Contains(child) then
//    Exit;
//  FChildList.Insert(0, child);
  AddObject(child);
  child.FX := FX + child.FLeft;
  child.FY := FY + child.FTop;
end;

procedure TUIElement.alignChildrenH;
begin

end;

procedure TUIElement.alignChildrenV;
var
  w, h: Single;
  i: Integer;
  ui: TUIElement;
begin
  h := FHeight / ChildrenCount;
  w := FWidth / 2;
  for i := 0 to ChildrenCount - 1 do
  begin
    ui := Self[i];
    ui.move(Trunc(w - ui.FWidth / 2), Trunc((i + 0.5) * h - ui.FHeight / 2));
  end;
end;

procedure TUIElement.clearBmps;
begin
  FFrameCount := 0;
  FillChar(FSprBmps, SizeOf(FSprBmps), 0);
end;

constructor TUIElement.Create(name: string; prnt: TUIElement; txt: string; l,
  t, w, h: Integer);
begin
  Create(name, prnt);
  setSize(w, h);
  setPostion(l, t);
  FText := txt;
end;

procedure TUIElement.dblClick;
begin
  if not FCanHit then
    Exit;
  if Assigned(FOnDblClick) then
    FOnDblClick(Self);
end;

constructor TUIElement.Create(name: string; prnt: TUIElement; l, t, w,
  h: Integer);
begin
  Create(name, prnt);
  setSize(w, h);
  setPostion(l, t);
end;

constructor TUIElement.Create(name: string; prnt: TUIElement);
begin
  inherited Create(FUIOwner);

  Self.Name := name;
  if Assigned(prnt) then
    prnt.addChild(Self);
  FEnabled := True;
  FVisible := True;
  FCanHit := True;
  FOpacity := 1.0;
  FScale.X := 1.0;
  FScale.Y := 1.0;
  FFontSize := UIDefaultFontSize;
  FFontColor := UIDefaultTextColor;
end;

destructor TUIElement.Destroy;
var
  child: TUIElement;
begin
//  if Assigned(FChildList) then
//  begin
//    for child in FChildList do
//      child.Free;
//    FreeAndNil(FChildList);
//  end;

  inherited;
end;

procedure TUIElement.draw;
var
  bmp: TGameSprBmp;
  rt, rt1, rt2, rtDst: TRectF;
  frame, frame0: TGameSprFrame;
  i: Integer;
  child: TUIElement;
  sizeList: array[0..8] of TPointF;
begin
  if not Assigned(FCanvas) or not FVisible then
    Exit;
  if (FWidth = 0) or (FHeight = 0) then
    Exit;

  if FFrameCount > 0 then
  begin
    if frmIdx >= 0 then
      FFrameIndex := frmIdx
    else if not FEnabled then
      FFrameIndex := FFrameCount - 1
    else if FDown or (Self = FDownElement) then
      FFrameIndex := 1
    else if Self = FOverElement then
      FFrameIndex := 2
    else
      FFrameIndex := 0;

    if FStretchMode = sm3Slot then
    begin
      for i := 0 to 2 do
      begin
        bmp := FSprBmps[i];
        if not Assigned(bmp) then
          Continue;
        if FFrameIndex > bmp.FrameCount - 1 then
          FFrameIndex := 0;
        frame := bmp.FrameList[FFrameIndex];
        rt := frame.rt;
        if i = 0 then
          rtDst := TRectF.Create(TPointF.Create(FX, FY), rt.Width, FHeight)
        else if i = 1 then
          rtDst := myBoundsF(FX + rtDst.Width, FY, FWidth - rtDst.Width * 2, FHeight)
        else
          rtDst := myBoundsF(FX + FWidth - rt.Width, FY, rt.Width, FHeight);

        FCanvas.DrawBitmap(bmp, rt, rtDst, FOpacity);
      end;
    end else
    if FStretchMode = smV3Slot then
    begin
      for i := 0 to 2 do
      begin
        bmp := FSprBmps[i];
        if not Assigned(bmp) then
          Continue;
        if FFrameIndex > bmp.FrameCount - 1 then
          FFrameIndex := 0;
        frame := bmp.FrameList[FFrameIndex];
        rt := frame.rt;
        if i = 0 then
          rtDst := myBoundsF(FX, FY, FWidth, rt.Height)
        else if i = 1 then
          rtDst := myBoundsF(FX, FY + rtDst.Height, FWidth, FHeight - rtDst.Height * 2)
        else
          rtDst := myBoundsF(FX, FY + FHeight - rt.Height, FWidth, rt.Height);

        FCanvas.DrawBitmap(bmp, rt, rtDst, FOpacity);
      end;
    end else
    if FStretchMode = sm9Slot then
    begin
      for i := 0 to 8 do
      begin
        bmp := FSprBmps[i];
        if not Assigned(bmp) then
          Continue;
        if FFrameIndex > bmp.FrameCount - 1 then
          FFrameIndex := 0;
        frame := bmp.FrameList[FFrameIndex];
        sizeList[i].X := frame.rt.Width;
        sizeList[i].Y := frame.rt.Height;
      end;
      for i := 0 to 8 do
      begin
        bmp := FSprBmps[i];
        if not Assigned(bmp) then
          Continue;
        if FFrameIndex > bmp.FrameCount - 1 then
          FFrameIndex := 0;
        frame := bmp.FrameList[FFrameIndex];
        rt := frame.rt;
        sizeList[i].X := rt.Width;
        sizeList[i].Y := rt.Height;
        case i of
          0:
            rtDst := TRectF.Create(TPointF.Create(FX, FY), rt.Width, rt.Height);
          1:
            rtDst := TRectF.Create(TPointF.Create(FX + sizeList[i-1].X, FY),
              FWidth - sizeList[i-1].X - sizeList[i+1].X, rt.Height);
          2:
            rtDst := TRectF.Create(TPointF.Create(FX + FWidth - rt.Width, FY), rt.Width, rt.Height);
          3:
            rtDst := TRectF.Create(TPointF.Create(FX, FY + sizeList[i-3].Y), rt.Width,
              FHeight - sizeList[i-3].Y - sizeList[i+3].Y);
          4:
            rtDst := TRectF.Create(TPointF.Create(FX + sizeList[i-1].X, FY + sizeList[i-3].Y),
              FWidth - sizeList[i-1].X - sizeList[i+1].X, FHeight - sizeList[i-3].Y - sizeList[i+3].Y);
          5:
            rtDst := TRectF.Create(TPointF.Create(FX + FWidth - rt.Width, FY + sizeList[i-3].Y),
              rt.Width, FHeight - sizeList[i-3].Y - sizeList[i+3].Y);
          6:
            rtDst := TRectF.Create(TPointF.Create(FX, FY + FHeight - rt.Height), rt.Width, rt.Height);
          7:
            rtDst := TRectF.Create(TPointF.Create(FX + sizeList[i-1].X, FY + FHeight - rt.Height),
              FWidth - sizeList[i-1].X - sizeList[i+1].X, rt.Height);
          8:
            rtDst := TRectF.Create(TPointF.Create(FX + FWidth - rt.Width, FY + FHeight - rt.Height),
              rt.Width, rt.Height);
        end;

        FCanvas.DrawBitmap(bmp, rt, rtDst, FOpacity);
      end;
    end else
    for bmp in FSprBmps do
    begin
      if not Assigned(bmp) then
        Continue;

      if FFrameIndex > bmp.FrameCount - 1 then
        FFrameIndex := 0;
      frame := bmp.FrameList[FFrameIndex];
      rt := frame.rt;
//      if FStretchMode = smFull then
//        rtDst := myBoundsF(FX, FY, FWidth, FHeight)
      if FStretchMode = smCenter then
        rtDst := myBoundsF(FX + (FWidth - rt.Width) / 2, FY + (FHeight - rt.Height) / 2, rt.Width, rt.Height)
      else if FStretchMode = smFit then
      begin
        if FWidth / FHeight <= bmp.Width / bmp.Height then
        begin
          rtDst.Height := bmp.Height * FWidth / bmp.Width;
          rtDst := myBoundsF(FX, FY + (FHeight - rtDst.Height) / 2, FWidth, rtDst.Height);
        end else
        begin
          rtDst.Width := bmp.Width * FHeight / bmp.Height;
          rtDst := myBoundsF(FX + (FWidth - rtDst.Width) / 2, FY, rtDst.Width, FHeight);
        end;
      end
      else begin
        if FFrameIndex > 0 then
        begin
          frame0 := bmp.FrameList[0];
          rtDst := myBoundsF(FX + (frame0.xoff - frame.xoff) * FScale.X, FY + (frame0.yoff - frame.yoff) * FScale.Y,
            rt.Width * FScale.X, rt.Height * FScale.Y);
        end else
          rtDst := myBoundsF(FX , FY , rt.Width * FScale.X, rt.Height * FScale.Y);
      end;
      FCanvas.DrawBitmap(bmp, rt, rtDst, FOpacity);
  {$IFDEF uidebug}
        FCanvas.Stroke.Color := TAlphaColorRec.Yellow;
        FCanvas.DrawRect(rtDst, 1);
  {$ENDIF}
    end;
  end else
    FillChar(rtDst, SizeOf(rtDst), 0);

  if FText <> '' then
  begin
    FCanvas.Font.Size := FFontSize;
    FCanvas.Font.Style := FFontStyle;
    FCanvas.Fill.Color := FFontColor;
    if FTextPosition = tpCenter then
      FCanvas.FillText(myBoundsF(FX + 2, FY, FWidth - 4, FHeight), FText, False,
        FOpacity, [], FTextAlign, FVTextAlign)
    else
      FCanvas.FillText(myBoundsF(FX + 2 + rtDst.Width, FY, FWidth - 4 - rtDst.Width, FHeight), FText, False,
        FOpacity, [], FTextAlign, FVTextAlign);
  end;

  {$IFDEF uidebug}
  FCanvas.Stroke.Color := TAlphaColorRec.Fuchsia;
  FCanvas.DrawRect(BoundsF, 1);
  {$ENDIF}

  for i := 0 to ChildrenCount - 1 do
    TUIElement(Children[i]).draw;
//  if Assigned(FChildList) then
//    for i := FChildList.Count - 1 downto 0 do
//      FChildList[i].draw;
end;

class procedure TUIElement.drawGameUI;
begin
  if not Assigned(gameUI) then
    Exit;
  gameUI.draw;
end;

class procedure TUIElement.FCheckTimerTimer(Sender: TObject);
begin
  uiCheckLongTap;
  uiEditChange(FFmxEdit);
end;

class function TUIElement.findUI(sname: string): Pointer;
begin
  Result := FUIOwner.FindComponent(sname);
end;

function TUIElement.Get(idx: Integer): TUIElement;
begin
  if (idx < 0) or (idx > ChildrenCount - 1) then
    idx := 0;
  Result := TUIElement(Children[idx]);
end;

function TUIElement.getBoundsF: TRectF;
begin
  Result := myBoundsF(FX, FY, FWidth, FHeight);
end;

function TUIElement.getClientRect: TRectF;
begin
  Result.Top := FTop;
  Result.Left := FLeft;
  Result.Width := FWidth;
  Result.Height := FHeight;
end;

function TUIElement.GetParent: TUIElement;
begin
  Result := TUIElement(FParent);
end;

function TUIElement.getPosition: TPoint;
begin
  Result.Create(FLeft, FTop);
end;

function TUIElement.GetVisible: Boolean;
begin
  if Assigned(Parent) and not Parent.Visible then
    Result := False
  else
    Result := FVisible;
end;

procedure TUIElement.hide;
begin
  FVisible := False;
end;

procedure TUIElement.load3slotBmps(spath: string);
var
  s: string;
  i :Integer;
const
  fnames: array[0..2] of string = ('left.png', 'middle.png', 'right.png');
begin
  Self.FStretchMode := sm3Slot;
  for i := 0 to 2 do
    Self.FSprBmps[i] := loadSprBmp(spath + fnames[i]);
  if not Assigned(Self.FSprBmps[0]) then
    Exit;
  Self.FFrameCount := Self.FSprBmps[0].FrameCount;
//  if Self.FFrameCount = 0 then
//    Exit;
//  Self.FWidth := 0;
//  for i := 0 to 2 do
//    Inc(Self.FWidth, Round(Self.FSprBmps[i].FrameList[0].rt.Width));
//  Self.FHeight := Round(Self.FSprBmps[0].FrameList[0].rt.Height);
end;

procedure TUIElement.load9slotBmps(spath: string);
var
  s: string;
  i, w, h: Integer;
const
  fnames: array[0..8] of string = (
    'top\left.png', 'top\middle.png', 'top\right.png',
    'middle\left.png', 'middle\middle.png', 'middle\right.png',
    'bottom\left.png', 'bottom\middle.png', 'bottom\right.png' );
begin
  Self.FStretchMode := sm9Slot;
  for i := 0 to 8 do
    Self.FSprBmps[i] := loadSprBmp(spath + fnames[i]);
  if not Assigned(Self.FSprBmps[0]) then
    Exit;
  Self.FFrameCount := Self.FSprBmps[0].FrameCount;
  if Self.FFrameCount = 0 then
    Exit;

  w := 0; h := 0;
  for i := 0 to 2 do
  begin
    Inc(w, Round(Self.FSprBmps[i].FrameList[0].rt.Width));
    Inc(h, Round(Self.FSprBmps[i*3].FrameList[0].rt.Height));
  end;
  if Self.FWidth < w then
    Self.FWidth := w;
  if Self.FHeight < h then
    Self.FHeight := h;
end;

procedure TUIElement.loadBmp(fname: string);
begin
  Self.FSprBmps[0] := loadSprBmp(fname);
  Self.FFrameCount := Self.FSprBmps[0].FrameCount;
  if Self.FFrameCount = 0 then
    Exit;
  if (FStretchMode = smNone) or (FWidth = 0) then
  begin
    Self.FWidth := Round(Self.FSprBmps[0].FrameList[0].rt.Width);
    Self.FHeight := Round(Self.FSprBmps[0].FrameList[0].rt.Height);
  end;
  setStretchMode(FStretchMode);
end;

procedure TUIElement.loadItemBmp;
begin
  if bigBmp then
    loadBmp(Format('item\item120\%d.png', [itmId]))
  else
    {$IFDEF gameclient}
    loadBmp(getItemFileName(itmId));
    {$ELSE}
    loadBmp(Format('item\item44\3%d.png', [itmId]));
    {$ENDIF}
end;

procedure TUIElement.loadObjBmp(objId: Integer);
begin
  if objId < 1000 then
    loadBmp(Format('misc\photo\hero\small\%d.png', [objId]))
  else
    loadBmp(Format('misc\photo\summon\%d.png', [objId]));
end;

procedure TUIElement.loadV3slotBmps(spath: string);
var
  s: string;
  i :Integer;
const
  fnames: array[0..2] of string = ('top.png', 'middle.png', 'bottom.png');
begin
  Self.FStretchMode := smV3Slot;
  for i := 0 to 2 do
    Self.FSprBmps[i] := loadSprBmp(spath + fnames[i]);
  if not Assigned(Self.FSprBmps[0]) then
    Exit;
  Self.FFrameCount := Self.FSprBmps[0].FrameCount;
end;

function TUIElement.mouseDown(btn: TMouseButton;sft: TShiftState; p: TPointF): Boolean;
var
  child: TUIElement;
  i: Integer;
begin
  Result := False;

  if not FVisible or not FEnabled or (not FCanHit) or
    not PtInRect(getClientRect, p) then
    Exit;

  p.X := p.X - FLeft;
  p.Y := p.Y - FTop;
  if ChildrenCount > 0 then
  for i := ChildrenCount - 1 downto 0 do
  begin
    child := TUIElement(Children[i]);
    if child.mouseDown(btn, sft, p) then
      Exit(True);
  end;

//  FDown := True;
  FDownElement := Self;
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, btn, sft, p.X, p.Y);
  Result := True;
end;

function TUIElement.mouseMove(sft: TShiftState; p: TPointF): Boolean;
var
  child: TUIElement;
  i: Integer;
begin
  Result := False;

  if not FVisible or not FEnabled or not FCanHit or not PtInRect(getClientRect, p) then
    Exit;

  p.X := p.X - FLeft;
  p.Y := p.Y - FTop;
  if ChildrenCount > 0 then
  for i := ChildrenCount - 1 downto 0 do
  begin
    child := TUIElement(Children[i]);
    if child.mouseMove(sft, p) then
      Exit(True);
  end;
  Result := True;
  FOverElement := Self;
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, sft, p.X, p.Y);
end;

function TUIElement.mouseUp(btn: TMouseButton; sft: TShiftState; p: TPointF): Boolean;
var
  child: TUIElement;
  i: Integer;
begin
  Result := False;
  if not FVisible or not FEnabled or not FCanHit or not PtInRect(getClientRect, p) then
    Exit;

  p.X := p.X - FLeft;
  p.Y := p.Y - FTop;
  if ChildrenCount > 0 then
  for i := ChildrenCount - 1 downto 0 do
  begin
    child := TUIElement(Children[i]);
    if child.mouseUp(btn, sft, p) then
      Exit(True);
  end;
  Result := True;
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, btn, sft, p.X, p.Y);
end;

procedure TUIElement.move(left, top: Integer);
var
  child: TUIElement;
  i: Integer;
begin
  FLeft := left; FTop := top;
  if Assigned(Parent) then
  begin
    FX := Parent.FX + left;
    FY := Parent.FY + top;
  end else
  begin
    FX := left;
    FY := top;
  end;

  if ChildrenCount > 0 then
  for i := ChildrenCount - 1 downto 0 do
  begin
    child := TUIElement(Children[i]);
    child.move(child.FLeft, child.FTop);
  end;

end;

class procedure TUIElement.setFmxEdit(const Value: TEdit);
begin
  FFmxEdit := Value;
  FFmxEdit.Font := FCanvas.Font;
  FFmxEdit.OnChange := uiEditChange;
  FFmxEdit.OnClick := uiEditChange;
  FFmxEdit.OnKeyUp := uiEditKeyUp;
  FFmxEdit.Caret.Visible := False;
end;

procedure TUIElement.SetHeight(const Value: Integer);
begin
  setSize(FWidth, Value);
end;

procedure TUIElement.SetLeft(const Value: Integer);
begin
  move(Value, FTop);
end;

procedure TUIElement.setPostion(left, top: Integer);
var
  child: TUIElement;
  i: Integer;
begin
  if Assigned(Parent) and (Parent.FWidth > 0) then
  begin
    if left < 0 then
      left := Parent.FWidth + left - FWidth;
    if top < 0 then
      top := Parent.FHeight + top - FHeight;
  end;

  FLeft := left; FTop := top;
  if Assigned(Parent) then
  begin
    FX := Parent.FX + left;
    FY := Parent.FY + top;
  end else
  begin
    FX := left;
    FY := top;
  end;

  if ChildrenCount > 0 then
  for i := ChildrenCount - 1 downto 0 do
  begin
    child := TUIElement(Children[i]);
      child.move(child.FLeft, child.FTop);
  end;
end;

procedure TUIElement.setOpacity(const Value: Single);
begin
  FOpacity := Value;
  if FOpacity < 0.01 then
    FVisible := False
  else
    FVisible := True;
end;

procedure TUIElement.SetParent(const Value: TUIElement);
begin
  FParent := Value;
end;

procedure TUIElement.setPostion(p: TUIPosition);
begin
  case p of
    upCenter:
      setPostion((Parent.FWidth - FWidth) div 2, (Parent.FHeight - FHeight) div 2);
    upTopCenter:
      setPostion((Parent.FWidth - FWidth) div 2, 0);
    upBottomCenter:
      setPostion((Parent.FWidth - FWidth) div 2, Parent.FHeight - FHeight);
    upTopLeft:
      setPostion(0, 0);
    upTopRight:
      setPostion(Parent.FWidth - FWidth, 0);
    upBottomLeft:
      setPostion(0, Parent.Height - FHeight);
    upBottomRight:
      setPostion(Parent.Width - FWidth, Parent.Height - FHeight);
    upLeftCenter:
      setPostion(0, (Parent.Height - FHeight) div 2);
    upRightCenter:
      setPostion(Parent.Width - FWidth, (Parent.Height - FHeight) div 2);
  end;
end;

procedure TUIElement.setSize(w, h: Integer);
var
  minW, minH: Integer;
  i: Integer;
  bmp: TGameSprBmp;
begin
  minW := 0; minH := 0;
  if FStretchMode = sm3Slot then
  begin
    for i := 0 to 2 do
    begin
      bmp := FSprBmps[i];
      if Assigned(bmp) and (bmp.FrameCount > 0) then
        minW := minW + Round(bmp.FrameList[0].rt.Width);
    end;
    if w < minW then
      w := minW;
    h := FHeight;
  end else
  if FStretchMode = sm9Slot then
  begin
    for i := 0 to 2 do
    begin
      bmp := FSprBmps[i];
      if Assigned(bmp) and (bmp.FrameCount > 0) then
        minW := minW + Round(bmp.FrameList[0].rt.Width);

      bmp := FSprBmps[3 * i];
      if Assigned(bmp) and (bmp.FrameCount > 0) then
        minH := minH + Round(bmp.FrameList[i].rt.Height);
    end;
    if w < minW then
      w := minW;
    if h < minH then
      h := minH;
  end;
  FWidth := w;
  FHeight := h;
  setStretchMode(FStretchMode);
end;

procedure TUIElement.setSprBmp(idx: Integer; sprBmp: TGameSprBmp);
begin
  if (idx > High(FSprBmps)) or (idx < 0) then
    Exit;
  FSprBmps[idx] := sprBmp;
  FFrameCount := sprBmp.FrameCount;
  if FFrameCount = 0 then
    Exit;
  FWidth := Round(sprBmp.FrameList[0].rt.Width);
  FHeight := Round(sprBmp.FrameList[0].rt.Height);
end;

procedure TUIElement.setStretchMode(const Value: TUIStrechMode);
var
  rt: TRectF;
begin
  FStretchMode := Value;
  if Value in [smLeft, smFull] then
  begin
    if not Assigned(FSprBmps[0]) or (FSprBmps[0].FrameCount = 0) then
      Exit;
    rt := FSprBmps[0].FrameList[0].rt;
    if (rt.Height = 0) or (rt.Width = 0) then
      Exit;
    FScale.Y := FHeight / rt.Height;
    if Value = smLeft then
      FScale.X := FScale.Y
    else
      FScale.X := FWidth / rt.Width;
  end;
end;

procedure TUIElement.SetTop(const Value: Integer);
begin
  move(FLeft, Value);
end;

procedure TUIElement.SetWidth(const Value: Integer);
begin
  setSize(Value, FHeight);
end;

procedure TUIElement.SetX(const Value: integer);
begin
  FX := Value;
end;

procedure TUIElement.setY(const Value: Integer);
begin
  FY := Value;
end;

procedure TUIElement.show;
begin
  FVisible := True;
end;

class procedure TUIElement.uiCheckLongTap;
begin
  if (FDownElement <> nil)
    and Assigned(FDownElement.OnDblClick)
    and (FDownElementPosition.Distance(FDownElement.getPosition) < 3)
    and (FDownTime <> 0) and (TThread.GetTickCount64 - FDownTime > 800) then
  begin
    FDownElement.dblClick;
    FDownElement := nil;
    FDownTime := 0;
  end;
end;

class procedure TUIElement.uiEditChange;
var
  edtFmx: TEdit;
  edt: TUIEdit;
begin
  if sender = nil then
    Exit;
  if (FEditElement <> nil) then
  begin
    edt := TUIEdit(FEditElement);
    edtFmx := TEdit(sender);
    if (edt.SelStart <> edtFmx.SelStart) or (edt.SelLength <> edtFmx.SelLength)
      or (edt.Text <> edtFmx.Text) then
    begin
      edt.SelStart := edtFmx.SelStart;
      edt.CaretPos := edtFmx.CaretPosition;
      edt.SelLength := edtFmx.SelLength;
      edt.Text := edtFmx.Text;
      edt.doChange;
    end;
  end;
end;

class procedure TUIElement.uiEditKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  uiEditChange(Sender);
  if Assigned(FEditElement) and Assigned(TUIEdit(FEditElement).FOnKeyUp) then
    TUIEdit(FEditElement).FOnKeyUp(Sender, Key, KeyChar, Shift);
end;

class function TUIElement.uiMouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Single): Boolean;
begin
  Result := False;
  if not Assigned(gameUI) then
    Exit;
  if gameUI.mouseDown(Button, Shift, TPointF.Create(x, y)) then
  begin
    Result := True;
    if Assigned(FDownElement) then
    begin
      if FDownElement.FCanMove then
        FDownElement.BringToFront;
      gameUI.FDownElementPosition := gameUI.FDownElement.getPosition;
    end;
    gameUI.FDownPoint := TPointF.Create(X, Y);
    FDownTime := TThread.GetTickCount64;
  end;
end;

class function TUIElement.uiMouseMove(Shift: TShiftState; X, Y: Single): Boolean;
var
  dx, dy: Integer;
begin
  if not Assigned(gameUI) then
    Exit(False);
  Result := gameUI.mouseMove(Shift, TPointF.Create(X, Y));
  if not Result then
    gameUI.FOverElement := nil;
  if Assigned(gameUI.FDownElement) and gameUI.FDownElement.FCanMove then
  begin
    dx := Trunc(X - gameUI.FDownPoint.X);
    dy := Trunc(Y - gameUI.FDownPoint.Y);
    gameUI.FDownElement.move(gameUI.FDownElementPosition.X + dx, gameUI.FDownElementPosition.Y + dy);
  end;
end;

class function TUIElement.uiMouseUp(Button: TMouseButton;
      Shift: TShiftState; X, Y: Single): Boolean;
begin
  if not Assigned(gameUI) then
    Exit(False);
  FEditElement := nil;
  Result := gameUI.mouseUp(Button, Shift, TPointF.Create(x, y));
  if not Assigned(FEditElement) and Assigned(FFmxEdit) then
    FFmxEdit.Visible := False;
  FDownElement := nil;
  FDownTime := 0;
  FUpTime := TThread.GetTickCount64;
end;

{ TUIButton }

constructor TUIButton.Create(name: string; prnt: TUIElement; l, t, w,
  h: Integer; txt: string; defBtnType: Integer);
begin
  inherited Create(name, prnt);
  FText := txt;
  FFontSize := UIDefaultFontSize;
  FFontColor := TAlphaColorRec.Black;
  FStretchMode := smFull;
  setSize(w, h);
  setPostion(l, t);
  if defBtnType in [0..1] then
    loadBmp(DefBtnImgFiles[defBtnType]);
end;

function TUIButton.mouseUp(btn: TMouseButton; sft: TShiftState; p: TPointF): Boolean;
begin
  Result := inherited;
  if not Result then
    Exit;
  if (Self = FDownElement) and Assigned(OnClick) then
    OnClick(Self);
end;

{ TUIPanel }

procedure TUIPanel.btnCloseClick(sender: TObject);
begin
  hide;
end;

constructor TUIPanel.Create;
begin
  inherited Create(name, prnt);

  FVisible := False;
  FCanMove := True;
  FStretchMode := sm9Slot;
  FVTextAlign := TTextAlign.Leading;
  FText := txt;
  setSize(w, h);
  if closeBtn then
  begin
    FBtnClose := TUIButton.Create(name + '_btnClose', Self, -4, 4, 27, 27, '', -1);
    FBtnClose.OnClick := btnCloseClick;
  end;
  if defBmps then
    loadBmps;
end;

procedure TUIPanel.hide;
begin
  inherited;
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

procedure TUIPanel.loadBmps(btnFile, bgFile: string);
begin
  load9slotBmps(bgFile);
  if Assigned(FBtnClose) then
    FBtnClose.loadBmp(btnFile);
end;

{ TUIScene }

constructor TUIScene.Create(name: string; prnt: TUIElement);
begin
  inherited;
  if not Assigned(FActiveScene) then
    FActiveScene := Self;
  if not Assigned(FCheckTimer) then
  begin
    FCheckTimer := TTimer.Create(nil);
    FCheckTimer.Interval := 50;
    FCheckTimer.OnTimer := FCheckTimerTimer;
  end;
end;

function TUIScene.mouseDown(btn: TMouseButton;sft: TShiftState; p: TPointF): Boolean;
var
  child: TUIElement;
  i: Integer;
begin
  Result := False;
  if not FVisible or not FEnabled or (not FCanHit) or
    not PtInRect(getClientRect, p) then
    Exit;

  if ChildrenCount > 0 then
  for i := ChildrenCount - 1 downto 0 do
  begin
    child := TUIElement(Children[i]);
    if child.mouseDown(btn, sft, p) then
      Exit(True);
  end;

end;

function TUIScene.mouseMove(sft: TShiftState; p: TPointF): Boolean;
var
  child: TUIElement;
  i: Integer;
begin
  Result := False;
  if not FVisible or not PtInRect(getClientRect, p) then
    Exit;

  if ChildrenCount > 0 then
  for i := ChildrenCount - 1 downto 0 do
  begin
    child := TUIElement(Children[i]);
    if child.mouseMove(sft, p) then
      Exit(True);
  end;
end;

function TUIScene.mouseUp(btn: TMouseButton; sft: TShiftState; p: TPointF): Boolean;
var
  child: TUIElement;
  i: Integer;
begin
  Result := False;
  if not FVisible or not PtInRect(getClientRect, p) then
    Exit;

  if ChildrenCount > 0 then
  for i := ChildrenCount - 1 downto 0 do
  begin
    child := TUIElement(Children[i]);
    if child.mouseUp(btn, sft, p) then
      Exit(True);
  end;
end;

{ TUIRichView }

procedure TUIRichView.addText;
var
  l, r, idx, i, len: Integer;
  rl: TRVLine;
  txtblk: TRVTextBlock;
  blk: TRVBlock;
  w0, w, h: Single;
  s1: string;

begin
  if s.Length = 0 then
    Exit;

  if FOriginalLines.Count > 0 then
    FOriginalLines[FOriginalLines.Count - 1] := FOriginalLines[FOriginalLines.Count - 1] + s
  else
    FOriginalLines.add(s);

  if FLines.Count > 0 then
    rl := FLines.Last
  else
    rl := addNewLine;

  blk := rl.getBlock(-1);
  if not FFontChanged and (blk <> nil) and (blk.blockType = rbtText) and
    (blk.script = scr) then
  begin
    txtblk := TRVTextBlock(blk);
  end else
  begin
    txtblk := rl.addTextBlock;
    txtblk.script := scr;
  end;

  FCanvas.Font.Size := FFont.Size;
  FCanvas.Font.Style := FFont.Style;
  w := FCanvas.TextWidth(s);
  h := FCanvas.TextHeight('国W');
  rl.FHeight := Max(h, rl.FHeight);
  if Round(w + txtblk.right) <= FWidth then
  begin
    txtblk.text := txtblk.text + s;
    txtblk.right := txtblk.right + w;
    rl.FWidth := rl.FWidth + w;
    Exit;
  end;

  w0 := FWidth - txtblk.right;

//  len := Trunc(w0 / FCanvas.Font.Size);
//  if len > s.Length then
//    len := s.Length;
  repeat
    l := 1;
    r := s.Length - 1;
    repeat
      i := (r + l) div 2;
      s1 := s.Substring(0, i);
      w := FCanvas.TextWidth(s1);
      if w > w0 then
        r := i
      else
        l := i;
    until r <= l + 1;
    s1 := s.Substring(0, r);
    w := FCanvas.TextWidth(s1);
    if w > w0 then
    begin
      i := l;
      s1 := s.Substring(0, i);
      w := FCanvas.TextWidth(s1);
    end else
      i := r;
    if i > 0 then
    begin
      txtblk.text := txtblk.text + s1;
      txtblk.right := txtblk.right + w;
      rl.FWidth := rl.FWidth + w;
      s := s.Remove(0, i);
    end;

    if s.Length = 0 then
      Break;

    rl := addNewLine;
    w0 := FWidth;
    rl.FHeight := h;
    txtblk := rl.addTextBlock;
    w := FCanvas.TextWidth(s);
    if w <= w0 then
    begin
      txtblk.text := s;
      txtblk.right := w;
      rl.FWidth := w;
      Break;
    end;
  until s.Length = 0;

end;

function TUIRichView.addNewLine;
var
  h: Single;
  i: Integer;
begin
  Result := TRVLine.Create(Self);
  FLines.Add(Result);
  Result.FHeight := FCanvas.TextHeight('国W');
end;

procedure TUIRichView.beginUpdate;
begin
  oldLineIndex := lineIndex;
  updating := True;
end;

procedure TUIRichView.clear;
var
  i: Integer;
begin
  for i := 0 to FLines.Count - 1 do
    FLines[i].Free;
  FLines.Clear;
//  paint;
end;

constructor TUIRichView.Create;
begin
  inherited Create(name, prnt);
  setSize(w, h);
  setPostion(l, t);
//  if img.Bitmap = nil then
//  FVScrollBar := vsb;
//  FBounds := myBoundsF(0, 0, FWidth, FHeight);
  FFont := TFont.Create;
  FFont.Size := UIDefaultFontSize;
  FOriginalLines := TStringList.Create;
  FLines := TList<TRVLine>.Create;

  FBgColor := bgClr;
  defTextColor := fclr;
  defFontSize := UIDefaultFontSize;
//  defTextColor := UIDefaultTextColor;
  defFontStyle := UIDefaultFontStyle;

  wordWrap := True;
  autoScroll := True;
end;

procedure TUIRichView.delete(idx: Integer);
begin

end;

destructor TUIRichView.Destroy;
var
  rl: TRVLine;
begin
  FFont.Free;
  FOriginalLines.Free;
  for rl in FLines do
    rl.Free;
  FLines.Free;
  inherited;
end;

procedure TUIRichView.endUpdate;
begin
  updating := False;
//  paint;
end;

procedure TUIRichView.hscrollBarChange(Sender: TObject);
begin

end;

function TUIRichView.mouseDown;
var
  idx: Integer;
  i: Integer;
  h: Single;
  blk: TRVBlock;
begin
  Result := inherited;
  if not Result then
    Exit;
//  FMouseDown := True;
  p.X := p.X - FLeft;
  p.Y := p.Y - FTop;
  FMouseDownPoint := p;
  FMouseDownLineIndex := topLineIndex;
  FActionBlk := nil;
//  if Assigned(FVScrollBar) then
//    FMouseDonwSBValue.Y := FVScrollBar.Value;
//  if Assigned(FHScrollBar) then
//    FMouseDonwSBValue.X := FHScrollBar.Value;

//  Result := False;
  if not Assigned(FOnAction) then
    Exit;
  if p.X < 0 then
    Exit;

  h := 0;
  if autoScroll then
  begin
    for i := topLineIndex to FLines.Count - 1 do
    begin
      h := h + FLines[i].FHeight;
      if h > FHeight then
        Break;
    end;
  end;

  idx := -1;
  if not autoScroll or (h < FHeight) then
  begin
    h := 0;
    for i := topLineIndex to FLines.Count - 1 do
      if InRange(p.Y, h, h + FLines[i].FHeight) then
      begin
        idx := i;
        Break;
      end else
      begin
        h := h + FLines[i].FHeight;
        if h > FHeight then
          Break;
      end;
  end else
  begin
    h := FHeight;
    for i := FLines.Count - 1 downto 0 do
      if InRange(p.Y, h - FLines[i].FHeight, h) then
      begin
        idx := i;
        Break;
      end else
      begin
        h := h - FLines[i].FHeight;
        if h < 0 then
          Break;
      end;
  end;

  if idx < 0 then
    Exit;

  blk := FLines[i].getBlock(p.X);
  if (blk <> nil) and (blk.script.Length > 0) then
    FActionBlk := blk;
end;

function TUIRichView.mouseMove;
var
  h, dy: Single;
  i, t0: Integer;
begin
  Result := inherited;
  if not Result then
    Exit;

  p.X := p.X - FLeft;
  p.Y := p.Y - FTop;
  if Self = FDownElement then
  begin
//    if Assigned(FVScrollBar) then
//    begin
//      FVScrollBar.Value := FMouseDonwSBValue.Y + (Y - FMouseDonwPoint.Y) * FVScrollBar.Max / FHeight;
//    end;
//    if Assigned(FHScrollBar) then
//    begin
//      FHScrollBar.Value := FMouseDonwSBValue.X + (X - FMouseDonwPoint.X) * FHScrollBar.Max / FHeight;
//    end;
    if FLines.Count = 0 then
      Exit;
    dy := (p.Y - FMouseDownPoint.Y) * 3;
    h := 0;
    if dy < 0 then
    begin
      for i := FLines.Count - 1 downto 0 do
      begin
        h := h + FLines[i].height;
        if h > FHeight then
          Break;
      end;
      t0 := Min(i + 1, FLines.Count - 1);
      for i := FMouseDownLineIndex to FLines.Count - 1 do
      begin
        dy := dy + FLines[i].height;
        if (dy >= 0) then
        begin
          t0 := Min(t0, i);
          if (t0 <> topLineIndex) then
          begin
            if Assigned(FVScrollBar) then
              FVScrollBar.Value := i / FLines.Count * FVScrollBar.Max;
            topLineIndex := t0;
//            paint;
            autoScroll := FLines.Count - 1 = bottomLineIndex;
          end;
          Break;
        end;
      end;
    end else
    begin
      for i := FMouseDownLineIndex downto 0 do
      begin
        dy := dy - FLines[i].height;
        if (dy < 0) then
        begin
          if (i <> topLineIndex) then
          begin
            if Assigned(FVScrollBar) then
              FVScrollBar.Value := i / FLines.Count * FVScrollBar.Max;
            topLineIndex := i;
//            paint;
            autoScroll := False;
          end;
          Break;
        end;
      end;
    end;

//    FLastMousePoint := p;
  end;
end;

function TUIRichView.mouseUp;
begin
  Result := inherited;
//  FMouseDown := False;
  if not Result then
    Exit;

  p.X := p.X - FLeft;
  p.Y := p.Y - FTop;
  if Assigned(FActionBlk) then
  begin
    if (Abs(p.Y - FMouseDownPoint.Y) < 10) then
      FOnAction(FActionBlk);
  end
  else if (Abs(p.Y - FMouseDownPoint.Y) < 10) then
    Result := False;
end;

procedure TUIRichView.draw;
var
  i, k: Integer;
  y: Single;
  blk: TRVBlock;
  rl: TRVLine;
begin
  if not FVisible then
    Exit;

  y := 0;
  if autoScroll then
  begin

    for i := topLineIndex to FLines.Count - 1 do
    begin
      y := y + FLines[i].FHeight;
      if y > FHeight then
        Break;
    end;
  end;


//  if FBgColor <> 0 then
//    FCanvas.DrawBitmap(FBgBmp, FBgBmp.BoundsF, myBoundsF(FX, FY, FWidth, FHeight), 1);
  FCanvas.Fill.Color := FBgColor;
  FCanvas.FillRect(myBoundsF(FX, FY, FWidth, FHeight), 0, 0, [], FOpacity);
  if FLines.Count > 0 then
  begin
    if (not autoScroll) or (y < FHeight) then
    begin
      y := 0;
      for i := topLineIndex to FLines.Count - 1 do
      begin
        rl := FLines[i];
        rl.draw(y);
        y := rl.height + y;
        if y >= FHeight then
        begin
          bottomLineIndex := i;
          Break;
        end;
      end;
    end else
    begin
      if Assigned(FVScrollBar) then
        FVScrollBar.Value := FVScrollBar.Max;
      autoScroll := True;
      bottomLineIndex := FLines.Count - 1;
      y := FHeight - FLines.Last.height;
      for i := FLines.Count - 1 downto 0 do
      begin
        rl := FLines[i];
        rl.draw(y);
        y := y - rl.height;
        if y <= 0 then
        begin
          topLineIndex := i;
          Break;
        end;
      end;
    end;
  end;

  if Self <> FDownElement then
    autoScroll := FLines.Count - 1 = bottomLineIndex;
end;

procedure TUIRichView.resetFont;
begin
  FFontChanged := (defFontSize <> FFont.Size) or (defTextColor <> FTextColor)
    or (defFontStyle <> FFont.Style);
  if not FFontChanged then
    Exit;
  FFont.Size := defFontSize;
  FTextColor := defTextColor;
  FFont.Style := defFontStyle;
end;

procedure TUIRichView.vscrollBarChange(Sender: TObject);
begin
  if FDownElement <> Self then
  begin
    topLineIndex := Trunc(FLines.Count * FVScrollBar.Value / FVScrollBar.Max);
//    paint;
  end;
end;

procedure TUIRichView.setFont;
begin
  if fclr = 0 then
    fclr := FTextColor;
  if fsize = 0 then
    fsize := FFont.Size;
  FFontChanged := (fsize <> FFont.Size) or (fclr <> FTextColor) or (fstyle <> FFont.Style);
  if not FFontChanged then
    Exit;

  FFont.Size := fsize;
  FTextColor := fclr;
  FFont.Style := fstyle;
end;

{ TRVLine }

function TRVLine.addTextBlock: TRVTextBlock;
begin
  Result := TRVTextBlock.Create(FParent);
  Result.size := FParent.FFont.Size;
  Result.color := FParent.FTextColor;
  Result.style := FParent.FFont.Style;
  Parent.FFontChanged := False;
  Result.left := FWidth;
  Result.right := FWidth;
  blockList.Add(Result);
end;

constructor TRVLine.Create;
begin
  FParent := redt;
  blockList := TList<TRVBlock>.Create;
end;

destructor TRVLine.Destroy;
var
  rb: TRVBlock;
begin
  for rb in blockList do
  begin
    rb.Free;
  end;
  blockList.Free;
  inherited;
end;

procedure TRVLine.draw(y: Single);
var
  rb: TRVBlock;
begin
  for rb in blockList do
    rb.draw(y, FHeight);
end;

function TRVLine.getBlock(x: Single): TRVBlock;
var
  i: Integer;
begin
  if x < 0 then
  begin
    if blockList.Count > 0 then
      Exit(blockList.Last)
    else
      Exit(nil);
  end;
  for i := 0 to blockList.Count - 1 do
    if InRange(x, blockList[i].left, blockList[i].right) then
      Exit(blockList[i]);

  Result := nil;
end;

{ TRVTextBlock }

procedure TRVTextBlock.draw(y, h: Single);
begin
  prnt.FCanvas.Font.Size := size;
  prnt.FCanvas.Font.Style := style;
  prnt.FCanvas.Fill.Color := color;
  if y < 0 then
    y := 0
  else if y + h > prnt.FHeight then
    h := prnt.FHeight - y;
  prnt.FCanvas.FillText(myBoundsF(prnt.FX+left, prnt.FY+y, right-left, h), text, False,
    prnt.FOpacity, [], TTextAlign.Leading, TTextAlign.Leading);
end;

{ TRVBlock }

constructor TRVBlock.Create(rdt: TUIRichView);
begin
  prnt := rdt;
end;

{ TUILabel }

constructor TUILabel.Create;
begin
  inherited Create(name, prnt);
  FCanHit := False;
  FText := txt;
  setPostion(x, y);
  setSize(w, h);
  FTextAlign := align;
  FFontSize := size;
  FFontStyle := style;
  FFontColor := clr;
end;

procedure TUILabel.draw;
begin
  if not FVisible then
    Exit;
  FCanvas.Font.Size := FFontSize;
  FCanvas.Font.Style := FFontStyle;
  FCanvas.Fill.Color := FFontColor;
  FCanvas.FillText(myBoundsF(FX, FY, FWidth, FHeight), FText, False,
    FOpacity, [], FTextAlign);
end;

{ TUIRectangle }

constructor TUIRectangle.Create(name: string; prnt: TUIElement; l, t, w,
  h: Integer; clr: TAlphaColor);
begin
  inherited Create(name, prnt);

  setSize(w, h);
  setPostion(l, t);
  FColor := clr;
  FCanMove := False;
end;

procedure TUIRectangle.draw;
var
  i: Integer;
begin
  if not FVisible then
    Exit;
  if FDrawType = 0 then
  begin
    FCanvas.Fill.Color := FColor;
    FCanvas.FillRect(myBoundsF(FX, FY, FWidth, FHeight), FXRadius, FYRadius, [], FOpacity);
  end else
  begin
    FCanvas.Stroke.Color := FColor;
    FCanvas.DrawRect(myBoundsF(FX, FY, FWidth, FHeight), FOpacity);
  end;

  for i := 0 to ChildrenCount - 1 do
    TUIElement(Children[i]).draw;
end;

{ TUIGrid }

constructor TUIGrid.Create(name: string; prnt: TUIElement; r, c, l, t, w,
  h: Integer);
var
  i: Integer;
begin
  inherited Create(name, prnt);
  FRowCount := r;
  SetLength(FRowHeights, r);
  FColCount := c;
  SetLength(FColWidths, c);
  setSize(w, h);
  setPostion(l, t);
  FCanMove := False;
  FPgSize := r * c;
  FLineColor := UIDefaultTextColor;
  FGridIndex := -1;
  FBgColor := $40000000;
  FFontColor := TAlphaColorRec.White;
  FFontSize := 22;
  FTextAlign := TTextAlign.Leading;

  FRectList := TList<TRectF>.Create;
  for i := 0 to r - 1 do
    FRowHeights[i] := FHeight div r;
  for i := 0 to c - 1 do
    FColWidths[i] := FWidth div c;
  resetRectList;
//  FPgDnBtn := TUIButton.Create(name+'_pd', Parent, l + FWidth, t, 27, 27, '', -1);
//  FPgDnBtn.OnClick := pgDnClick;
//  FPgUpBtn := TUIButton.Create(name+'_pu', Parent, l + FWidth, t + 27, 27, 27, '', -1);
//  FPgUpBtn.OnClick := pgUpClick;
end;

destructor TUIGrid.Destroy;
begin
  SetLength(FRowHeights, 0);
  SetLength(FColWidths, 0);
  FRectList.Free;
  inherited;
end;

procedure TUIGrid.draw;
var
  idx, i, w, h: Integer;
  rt: TRectF;
begin
  if not FVisible then
    Exit;
  if (FBgColor <> 0) then
  begin
    FCanvas.Fill.Color := FBgColor;
    FCanvas.FillRect(BoundsF, FOpacity)
  end;

  FCanvas.Fill.Color := FFontColor;
  FCanvas.Font.Size := FFontSize;
  for i := 0 to FPgSize - 1 do
    drawGrid(FTopIndex + i);
  if FLineColor <> 0 then
  begin
    FCanvas.Stroke.Color := FLineColor;
    FCanvas.DrawRect(myBoundsF(FX, FY, FWidth, FHeight), 1);
    h := 0;
    for i := 0 to FRowCount - 2 do
    begin
      Inc(h, FRowHeights[i]);
      FCanvas.DrawLine(TPointF.Create(FX, FY + h), TPointF.Create(FX + FWidth - 1, FY + h), 1);
    end;
    w := 0;
    for i := 0 to FColCount - 2 do
    begin
      Inc(w, FColWidths[i]);
      FCanvas.DrawLine(TPointF.Create(FX + w, FY), TPointF.Create(FX + w, FY + FHeight - 1), 1);
    end;
  end;
  idx := FGridIndex - FTopIndex;
  if InRange(idx, 0, FPgSize - 1) then
  begin
    FCanvas.Fill.Color := TAlphaColorRec.Red;
    if not FRowSelect then
    begin
      rt := FRectList[idx];
      rt.Offset(FX, FY);
      FCanvas.FillRect(rt, 0.2)
    end
    else begin
      w := idx - idx mod FColCount;
      for i := w to w + FColCount - 1 do
      begin
        rt := FRectList[i];
        rt.Offset(FX, FY);
        FCanvas.FillRect(rt, 0.2)
      end;
    end;
  end;
end;

function TUIGrid.getColIndex: Integer;
begin
  if FGridIndex < 0 then
    Result := -1
  else
    Result := FGridIndex mod FColCount;
end;

function TUIGrid.GetColWidths(idx: Integer): Integer;
begin
  Result := FColWidths[idx];
end;

function TUIGrid.getPgSize: Integer;
begin
  Result := FRowCount * FColCount;
end;

function TUIGrid.GetRowHeights(idx: Integer): Integer;
begin
  Result := FRowHeights[idx];
end;

function TUIGrid.getRowIndex: Integer;
begin
  if FGridIndex < 0 then
    Result := -1
  else
    Result := (FGridIndex + FTopIndex) div FColCount;
end;

function TUIGrid.getSelected: TUIElement;
begin
  if not InRange(FGridIndex, 0, FPgSize - 1) then
    Exit(nil);
  Result := ChildList[FGridIndex];
end;

function TUIGrid.mouseDown(btn: TMouseButton; sft: TShiftState;
  p: TPointF): Boolean;
var
  idx: Integer;
  i: Integer;
  h: Single;
  blk: TRVBlock;
begin
//  Result := inherited;
//  if not Result then
//    Exit;
  if not FVisible or not FEnabled or (not FCanHit) or
    not PtInRect(getClientRect, p) then
    Exit(False);
  Result := True;
  FDownElement := Self;
  p.X := p.X - FLeft;
  p.Y := p.Y - FTop;
  FMouseDownPoint := p;
  FMouseDownTopIndex := FTopIndex;
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, btn, sft, p.X, p.Y);
end;

function TUIGrid.mouseMove(sft: TShiftState; p: TPointF): Boolean;
var
  dy: Single;
  i: Integer;
begin
  Result := inherited;
  if not Result then
    Exit;

  p.X := p.X - FLeft;
  p.Y := p.Y - FTop;
  if Self = FDownElement then
  begin
//    if Assigned(FVScrollBar) then
//    begin
//      FVScrollBar.Value := FMouseDonwSBValue.Y + (Y - FMouseDonwPoint.Y) * FVScrollBar.Max / FHeight;
//    end;
//    if Assigned(FHScrollBar) then
//    begin
//      FHScrollBar.Value := FMouseDonwSBValue.X + (X - FMouseDonwPoint.X) * FHScrollBar.Max / FHeight;
//    end;
    if getCount < FPgSize then
      Exit;
    dy := (p.Y - FMouseDownPoint.Y) * 3;
    if dy < 0 then
    begin
      if getCount - FTopIndex <= FPgSize  then
        Exit;
    end else
    begin
      if FTopIndex = 0 then
        Exit;
    end;
    i := FMouseDownTopIndex - Trunc(dy / FHeight * FRowCount) * FColCount;
    if i <> FTopIndex then
    begin
      FTopIndex := i;
//      doResetGrids;
    end;

    FLastMousePoint := p;
  end;
end;

function TUIGrid.mouseUp(btn: TMouseButton; sft: TShiftState;
  p: TPointF): Boolean;
var
  i, idx, idx0: Integer;
  x0, y0: Single;

begin
  Result := False;
  if not FVisible or not FEnabled or not PtInRect(getClientRect, p) then
    Exit;

  p.X := p.X - FLeft;
  p.Y := p.Y - FTop;

  idx := -1;
  for i := 0 to FPgSize - 1 do
    if PtInRect(FRectList[i], p) then
    begin
      idx := FTopIndex + i;
      Break;
    end;
  if idx < 0 then
    Exit;

  p := FMouseDownPoint;
  idx0 := -1;
  for i := 0 to FPgSize - 1 do
    if PtInRect(FRectList[i], p) then
    begin
      idx0 := FTopIndex + i;
      Break;
    end;

  if btn = TMouseButton.mbRight then
  else begin
    if (idx = idx0) then
    begin
      FGridIndex := idx;
      if TThread.GetTickCount64 - FUpTime < 350 then
      begin
        if Assigned(FOnGridDblClick) then
          FOnGridDblClick(Self, idx);
      end else
        if Assigned(FOnGridClick) then
          FOnGridClick(Self, idx);
    end;
  end;
  Result := True;
end;

procedure TUIGrid.resetRectList;
var
  k, i: Integer;
  rt: TRectF;
begin
  FRectList.Clear;
  FillChar(rt, SizeOf(rt), 0);
  for i := 0 to FRowCount - 1 do
  begin
    rt.Bottom := rt.Top + FRowHeights[i] - 1;
    rt.Left := 0;
    for k := 0 to FColCount - 1 do
    begin
      rt.Right := rt.Left + FColWidths[k] - 1;
      FRectList.Add(rt);
      rt.Left := rt.Right + 1;
    end;
    rt.Top := rt.Bottom + 1;
  end;
end;

procedure TUIGrid.SetColWidths(idx: Integer; const Value: Integer);
var
  w: Integer;
begin
  w := FColWidths[idx] - Value;
  FColWidths[idx] := Value;
  Inc(FColWidths[idx+1], w);
  resetRectList;
end;

procedure TUIGrid.SetRowHeights(idx: Integer; const Value: Integer);
var
  w: Integer;
begin
  w := FRowHeights[idx] - Value;
  FRowHeights[idx] := Value;
  Inc(FRowHeights[idx+1], w);
  resetRectList;
end;

procedure TUIGrid.setTextAlign(const Value: TTextAlign);
var
  i: Integer;
begin
  FTextAlign := Value;
  for i := 0 to FPgSize - 1 do
  begin
    if ChildList[i] <> nil then
      ChildList[i].TextAlign := Value;
  end;
end;

procedure TUIGrid.setTopIndex(const Value: Integer);
var
  idx0, topIdx0: Integer;
begin
  if Value = FTopIndex then
    Exit;
  idx0 := FGridIndex;
  topIdx0 := FTopIndex;
  FTopIndex := Value;
  if FTopIndex > getCount - FPgSize then
    FTopIndex := getCount - FPgSize;
  FGridIndex := idx0 - FTopIndex + topIdx0;
//  doResetGrids;
end;

{ TUIEdit }

constructor TUIEdit.Create(name: string; prnt: TUIElement; l, t, w,
  h: Integer; clr: TAlphaColor);
begin
  inherited Create(name, prnt);
  setSize(w, h);
  setPostion(l, t);
  FFontColor := clr;
  FTextAlign := TTextAlign.Leading;
end;

procedure TUIEdit.doChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TUIEdit.draw(idx: Integer);
var
  s: string;
  l, t, w, h: Single;
  rt: TRectF;
begin
  inherited;
  //Draw selection
  if FSelLength > 0 then
  begin
    s := FText.Substring(FSelStart, FSelLength);
    w := FCanvas.TextWidth(s);
    s := FText.Substring(0, FSelStart);
  end else
  begin
    w := 3;
    s := FText.Substring(0, FCaretPos);
  end;
  h := FCanvas.TextHeight(FText);
  l := FCanvas.TextWidth(s);
  if l > FWidth - 3 then
    l := FWidth - 3;
  t := (FHeight - h) / 2;
  if l + w > FWidth then
    w := FWidth - l;
  rt := myBoundsF(FX + l, FY + t, w, h);
  FCanvas.Fill.Color := TAlphaColorRec.Aqua;
  FCanvas.FillRect(rt, 0.4);

  //draw rect if no bg img
  if FFrameCount > 0 then
    Exit;
  FCanvas.Stroke.Color := FFontColor;
  FCanvas.DrawRect(BoundsF, 1);
end;

function TUIEdit.getValue: Integer;
begin
  Result := StrToIntDef(FText, 0);
end;

function TUIEdit.mouseUp(btn: TMouseButton; sft: TShiftState;
  p: TPointF): Boolean;
var
  m: TMatrix;
begin
  Result := inherited;
  if not Result then
    Exit;

  if Self <> FEditElement then
  begin
    FEditElement := Self;
    m := FCanvas.Matrix;
    FFmxEdit.Position.X := FX * m.m11 + m.m31; //FX;
    FFmxEdit.Position.Y := FY * m.m22 + m.m32 ; //FY;
    FFmxEdit.Width := FWidth * m.m11;
    FFmxEdit.Height := FHeight * m.m22;
    FFmxEdit.Font.Size := FFontSize * m.m11;
    FFmxEdit.TextAlign := FTextAlign;
    FFmxEdit.Visible := True;
    FFmxEdit.SetFocus;
    FFmxEdit.Text := FText;
  end;
end;

procedure TUIEdit.setText(const Value: string);
begin
  FText := Value;
  if Self = FEditElement then
    FFmxEdit.Text := FText;
end;

procedure TUIEdit.setValue(const Value: Integer);
begin
  FText := Value.ToString;
  if Self = FEditElement then
    FFmxEdit.Text := FText;
end;

{ TUICheckbox }

constructor TUICheckbox.Create(name: string; prnt: TUIElement; txt: string; l,
  t, w, h: Integer);
begin
  inherited;
  FStretchMode := smLeft;
  FTextPosition := TUITextPosition.tpBmpRight;
  loadBmp(DefChkImgFile);
end;

procedure TUICheckbox.draw(frmIdx: Integer);
var
  i: Integer;
begin
  if not FDown then
    i := Byte(Self = FOverElement)
  else
    i := 2 + Byte(Self <> FOverElement);
  inherited draw(i);
end;

function TUICheckbox.mouseUp(btn: TMouseButton; sft: TShiftState;
  p: TPointF): Boolean;
begin
  Result := inherited;

  if Result then
    FDown := not FDown;
end;

{ TUISpinBox }

procedure TUISpinBox.btnDnClick(sender: TObject);
begin
  Value := Value - 1;
end;

procedure TUISpinBox.btnUpClick(sender: TObject);
begin
  Value := Value + 1;
end;

constructor TUISpinBox.Create(name: string; prnt: TUIElement; txt: string; l, t,
  w, h: Integer);
begin
  inherited Create(name, prnt, '', l, t, w, h);

  FBtnDn := TUIButton.Create(name + '_btnDn', Self, 0, 0, h, h, '－');
  FBtnDn.OnClick := btnDnClick;
  FBtnUp := TUIButton.Create(name + '_btnUp', Self, w - h, 0, h, h, '＋');
  FBtnUp.OnClick := btnUpClick;
  FEdit := TUIEdit.Create(name + '_edt', Self, h, 0, w - h * 2, h, TAlphaColorRec.White);
  FEdit.TextAlign := TTextAlign.Center;
  FEdit.Text := txt;
end;

function TUISpinBox.getText: string;
begin
  Result := FEdit.FText;
end;

function TUISpinBox.getValue: Integer;
begin
  Result := FEdit.Value;
end;

procedure TUISpinBox.setValue(const Value: Integer);
begin
  if Self.Value <> Value then
  begin
    FEdit.Value := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TUISpinBox.ValueInc;
begin
  Value := Value + 1;
end;

{ TUIAni }

constructor TUIAni.Create(name: string; prnt: TUIElement; l, t, w, h: Integer);
begin
  inherited;
  FDuration := 900;
end;

procedure TUIAni.draw(idx: Integer);
var
  t: Int64;
  bmp: TGameSprBmp;
  frame: TGameSprFrame;
  rtDst, rt: TRectF;

begin
  if not FVisible then
    Exit;
  if FFrameCount > 0 then
  begin
    t := TThread.GetTickCount64;
    idx := (t - FLastDrawTime) div (FDuration div FFrameCount) mod FFrameCount;
    FLastDrawTime := t - (t - FLastDrawTime) mod FDuration;
  end
  else
    idx := 0;
  FFrameIndex := idx;
  for bmp in FSprBmps do
  begin
    if not Assigned(bmp) then
      Continue;

    if FFrameIndex > bmp.FrameCount - 1 then
      FFrameIndex := 0;
    frame := bmp.FrameList[FFrameIndex];
    rt := frame.rt;

    rtDst := myBoundsF(FX - frame.xoff * FScale.X, FY - frame.yoff * FScale.Y,
      rt.Width * FScale.X, rt.Height * FScale.Y);
    FCanvas.DrawBitmap(bmp, rt, rtDst, FOpacity);
  {$IFDEF uidebug}
    FCanvas.Stroke.Color := TAlphaColorRec.Yellow;
    FCanvas.DrawRect(rtDst, 1);
  {$ENDIF}
  end;
end;

{ TUIComboBox }

constructor TUIComboBox.Create;
begin
  inherited;
  FItemList := TUIListbox.Create(name + '_itmlst', FActiveScene, 4, 1, l, t+h, Max(120, w), 160);
  FItemList.BgColor := $C0000000;
  FItemList.FTextAlign := TTextAlign.Leading;
  FItemList.hide;
  FItemList.OnGridClick := OnItemClick;
  OnClick := OnSelfClick;
end;

function TUIComboBox.getItemIndex: Integer;
begin
  Result := FItemList.ItemIndex;
end;

function TUIComboBox.getItems: TStrings;
begin
  Result := FItemList.Items;
end;

procedure TUIComboBox.OnItemClick(sender: TObject; idx: Integer);
begin
  FItemList.hide;
  ItemIndex := idx;
end;

procedure TUIComboBox.OnSelfClick(sender: TObject);
var
  t, l: Integer;
begin

  FItemList.Visible := not FItemList.Visible;
  if FItemList.Visible then
  begin
    if FY + FHeight + FItemList.Height <= FCanvas.Height then
      t := FY + FHeight
    else
      t := FY - FItemList.Height;
    if FY + FItemList.Width <= FCanvas.Width then
      l := FX
    else
      l := FCanvas.Width - FItemList.Width;
    FItemList.move(l, t);
    FItemList.BringToFront;
    if not InRange(FItemList.ItemIndex, FItemList.TopIndex, FItemList.TopIndex + FItemList.PgSize - 1) then
      FItemList.TopIndex := FItemList.ItemIndex;
  end;
end;

procedure TUIComboBox.setItemIndex(const Value: Integer);
begin
  if InRange(Value, 0, FItemList.Count - 1) then
    FText := FItemList.Items[Value];
  FItemList.ItemIndex := Value;
end;

{ TUITreeView }

function TUITreeView.add(txt: string): TUITreeViewNode;
begin
  Result := FItems.Add(TUITreeViewItem.Create(txt, FItemSize.cx, FItemSize.cy));
end;

function TUITreeView.addChild(node: TUITreeViewNode;
  txt: string): TUITreeViewNode;
begin
  Result := node.Add(TUITreeViewItem.Create(txt, FItemSize.cx, FItemSize.cy));
end;

constructor TUITreeView.Create(name: string; prnt: TUIElement; l, t, w,
  h: Integer);
begin
  inherited;
  FItems := TUITreeViewNode.Create;
  FItemSize.cx := 204;
  FItemSize.cy := 31;
  FBtnBmp := loadSprBmp(DefBtnImgFiles[1]);
  OnMouseDown := tvMouseDown;
end;

destructor TUITreeView.Destroy;
  procedure freeNode(node: TUITreeViewNode);
  var
    i: Integer;
  begin
    for i := 0 to node.Count - 1 do
      freeNode(node[i]);

    if node.Data <> nil then
      node.Data.Free;
  end;
begin
  freeNode(FItems);
  FItems.Free;
  inherited;
end;

procedure TUITreeView.draw;
var
  i: Integer;
  item: TUITreeViewItem;
  t, l: Single;

  function drawItem(tv: TUITreeView; item: TUITreeViewNode; var x0, y0: Single): Boolean;
  var
    rt: PRectF;
    i: Integer;
  begin
    if y0 > tv.FY + tv.Height then
      Exit(False);
    rt := @item.Data.FRect;
    rt.SetLocation(x0 + (item.Level -1) * 10, y0);

    if item.Count > 0 then
       tv.FCanvas.DrawBitmap(tv.FBtnBmp, tv.FBtnBmp.FrameList[0].rt, rt^, 1);
    if item.Data.FData <> '' then
      tv.FCanvas.FillText(InflateRectF(rt^, -3, -3), item.Data.FData, False, 1, [], TTextAlign.Leading);
    y0 := y0 + tv.FItemSize.cy;
    if item.Data.FExpanded then
    for i := 0 to item.Count - 1 do
      if not drawItem(tv, item[i], x0, y0) then
        Exit(False);

    Result := True;
  end;
begin
  inherited;
  if not FVisible then
    Exit;
  FCanvas.Stroke.Color := FontColor;
  FCanvas.DrawRect(BoundsF, FOpacity);
  FCanvas.Fill.Color := FontColor;
//  FCanvas.FillText(myBoundsF(FX, FY, Width, 33), );

  t := FY;
  l := FX + 5;
  for i := 0 to FItems.Count - 1 do
    if not drawItem(Self, FItems[i], l, t) then
      Break;

end;

function TUITreeView.mouseUp(btn: TMouseButton; sft: TShiftState;
  p: TPointF): Boolean;

var
  node: TUITreeViewNode;
  pt: TPointF;
  function getPtItem(var pt: TPointF; node: TUITreeViewNode): TUITreeViewNode;
  var
    child: TUITreeViewNode;
    i: Integer;
  begin
    if Assigned(node.Data) and PtInRect(node.Data.FRect, pt) then
      Exit(node);

    if not Assigned(node.Data) or node.Data.FExpanded then
      for i := 0 to node.Count - 1 do
      begin
        Result := getPtItem(pt, node[i]);
        if Assigned(Result) then
          Exit(Result);
      end;

    Result := nil;
  end;
begin
  Result := inherited;
  if not Result then
    Exit;
//  pt := p;
  p.Offset(-Left, -Top);
  if (Abs(p.Y - FTVDownPt.Y) > 10) or (Abs(p.Y - FTVDownPt.Y) > 10) then
    Exit;
  p.Offset(FX, FY);
  node := getPtItem(p, FItems);
  if Assigned(node) and Assigned(node.Data) then
  begin
    if node.Count > 0 then
      node.Data.FExpanded := not node.Data.FExpanded
    else if Assigned(FOnClickItem) then
      FOnClickItem(node.Data);
  end;
end;

procedure TUITreeView.tvMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FTVDownPt.X := X;
  FTVDownPt.Y := Y;
end;

{ TUITreeViewItem }

constructor TUITreeViewItem.Create;
begin
  FData := txt;
  FRect.Width := w;
  FRect.Height := h;
end;


{ TUIListbox }

procedure TUIListbox.clear;
begin
  FGridIndex := -1;
  FItems.Clear;
end;

constructor TUIListbox.Create(name: string; prnt: TUIElement; r, c, l, t, w,
  h: Integer);
var
  i: Integer;
begin
  inherited;
  FItems := TStringList.Create;
end;

destructor TUIListbox.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TUIListbox.drawGrid(idx: Integer);
var
  rt: TRectF;
begin
  if not InRange(idx, 0, FItems.Count - 1) then
    Exit;
  rt := FRectList[idx - FTopIndex];
  rt.Offset(X, Y);
  rt.Inflate(-3, 0);
  FCanvas.FillText(rt, FItems[idx], False, FOpacity, [], FTextAlign);
end;

function TUIListbox.getCount: Integer;
begin
  Result := FItems.Count;
end;

{ TUIListView }

function TUIListView.Add: TUIListItem;
begin
  Result := TUIListItem.Create(Self);
  FItems.Add(Result);
end;

procedure TUIListView.clear;
begin
  FGridIndex := -1;
  FItems.Clear;
end;

constructor TUIListView.Create(name: string; prnt: TUIElement; r, c, l, t, w,
  h: Integer);
begin
  inherited;
  FDuration := 1000;
  FItems := TObjectList<TUIListItem>.Create;
end;

destructor TUIListView.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TUIListView.drawGrid(idx: Integer);
var
  t: Int64;
  bmp: TGameSprBmp;
  frame0, frame: TGameSprFrame;
  rtDst, rt, rt0: TRectF;
  dw, dh: Single;
  x0, y0, w, h: Single;
  li: TUIListItem;
  drawImageOver, drawTextOver: Boolean;
begin
  if not InRange(idx, 0, FItems.Count - 1) then
    Exit;
  li := FItems[idx];
  if not li.Visible then
    Exit;
  rt0 := FRectList[idx - FTopIndex];
  w := rt0.Width; h := rt0.Height;
  x0 := FX + rt0.Left;
  y0 := FY + rt0.Top;
  if li.FFrameCount > 0 then
  begin
    t := TThread.GetTickCount64;
    idx := (t - li.FLastDrawTime) div (FDuration div li.FFrameCount) mod li.FFrameCount;
    li.FLastDrawTime := t - (t - li.FLastDrawTime) mod FDuration;
  end
  else
    idx := 0;
  li.FFrameIndex := idx;

  rt0.Offset(FX, FY);
  drawImageOver := False;
  if Assigned(FOnDrawItemImage) then
    FOnDrawItemImage(li, rt0, FCanvas, drawImageOver);

  if not drawImageOver then
  for bmp in li.FSprBmps do
  begin
    if not Assigned(bmp) then
    begin
      Continue;
    end;

    if li.FFrameIndex > bmp.FrameCount - 1 then
      li.FFrameIndex := 0;
    frame := bmp.FrameList[li.FFrameIndex];
    rt := frame.rt;
    dh := (h - rt.Height) / 2 * FScale.Y;
    dw := (li.ImgSize - rt.Width) / 2 * FScale.X;

    if li.FFrameIndex > 0 then
    begin
      frame0 := bmp.FrameList[0];
      rtDst := myBoundsF(x0 + dw + (frame0.xoff - frame.xoff) * FScale.X,
        y0 + dh + (frame0.yoff - frame.yoff) * FScale.Y,
        rt.Width * FScale.X, rt.Height * FScale.Y);
    end
    else
//      rtDst := myBoundsF(FX + (FWidth - rt.Width) / 2, FY + (FHeight - rt.Height) / 2, rt.Width, rt.Height)
      rtDst := myBoundsF(x0 + dw, y0 + dh, rt.Width * FScale.X, rt.Height * FScale.Y);
    FCanvas.DrawBitmap(bmp, rt, rtDst, FOpacity);

  {$IFDEF uidebug}
    FCanvas.Stroke.Color := TAlphaColorRec.Yellow;
    FCanvas.DrawRect(rtDst, 1);
  {$ENDIF}
  end;

  drawTextOver := False;
  if Assigned(FOnDrawItemText) then
    FOnDrawItemText(li, rt0, FCanvas, drawTextOver);
  if not drawTextOver then
  begin
    w := w - li.ImgSize - 4;
    h := h / 2;
    if li.FValue <> '' then
      FCanvas.FillText(myBoundsF(x0 + li.ImgSize + 2, y0 + h, w, h),
        li.FValue, True, FOpacity, [], FTextAlign);
    if li.FText <> '' then
      FCanvas.FillText(myBoundsF(x0 + li.ImgSize + 2, y0, w, h),
        li.FText, True, FOpacity, [], FTextAlign);
  end;
end;

function TUIListView.getCount: Integer;
begin
  Result := FItems.Count;
end;

function TUIListView.getFocusedItem: TUIListItem;
begin
  if FGridIndex < 0 then
    Exit(nil);
  Result := FItems[FGridIndex];
end;

procedure TUIListView.hideAll;
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
    FItems[i].Visible := False;
end;

{ TUIListItem }
constructor TUIListItem.Create;
begin
  FVisible := True;
  FParent := AParent;
  FImgSize := 60;
end;

procedure TUIListItem.loadBmp(fname: string);
begin
  Self.FSprBmps[0] := loadSprBmp(fname);
  if Assigned(Self.FSprBmps[0]) then
    Self.FFrameCount := Self.FSprBmps[0].FrameCount;
end;

procedure TUIListItem.loadItemBmp(itmId: Integer; bigBmp: Boolean);
begin
  if bigBmp then
    loadBmp(Format('item\item120\%d.png', [itmId]))
  else
    {$IFDEF gameclient}
    loadBmp(getItemFileName(itmId));
    {$ELSE}
    loadBmp(Format('item\item44\3%d.png', [itmId]));
    {$ENDIF}
end;

procedure TUIListItem.loadObjBmp(objId: Integer);
begin
  if objId < 1000 then
    loadBmp(Format('misc\photo\hero\small\%d.png', [objId]))
  else
    loadBmp(Format('misc\photo\summon\%d.png', [objId]));
end;

initialization
finalization
  if Assigned(TUIElement.FCheckTimer) then
    TUIElement.FCheckTimer.Free;

end.
