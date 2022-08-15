{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2021 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit gameAniUnit;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.UITypes, System.Rtti, System.Generics.Collections, FMX.Types, FMX.Graphics
  , System.SyncObjs, System.Math.Vectors;

type

  TGameAnimation = class;

  ITriggerAnimation = interface
  ['{8A291102-742F-45CB-9159-4E1D283A0414}']
    procedure StartTriggerAnimation(const AInstance: TFmxObject; const APropertyName: string);
    procedure StartTriggerAnimationWait(const AInstance: TFmxObject; const APropertyName: string);
  end;

  /// <summary>Helpers class for quick using animation. Don't use synchronous animation on the Android.
  /// Android uses asynchronous animation model.</summary>
  TGameAnimator = class
  private type
    IAnimationDestroyer = interface
    ['{3597F657-95E3-4E21-992D-C834EE540414}']
      procedure RegisterAnimation(const AAnimation: TGameAnimation);
    end;
    TAnimationDestroyer = class(TInterfacedObject, IFreeNotification, IAnimationDestroyer)
    private
      FAnimations: TList<Pointer>;
      procedure DoAniFinished(Sender: TObject);
      { IFreeNotification }
      procedure FreeNotification(AObject: TObject);
    public
      constructor Create;
      destructor Destroy; override;
      procedure RegisterAnimation(const AAnimation: TGameAnimation);
    end;
  private class var
    FDestroyer: IAnimationDestroyer;
  private
    class procedure CreateDestroyer;
    class procedure Uninitialize;
  public
    { animations }
    class procedure StartAnimation(const Target: TFmxObject; const AName: string);
    class procedure StopAnimation(const Target: TFmxObject; const AName: string);
    class procedure StartTriggerAnimation(const Target: TFmxObject; const AInstance: TFmxObject; const APropertyName: string);
    class procedure StartTriggerAnimationWait(const Target: TFmxObject; const AInstance: TFmxObject; const APropertyName: string);
    class procedure StopTriggerAnimation(const Target: TFmxObject; const AInstance: TFmxObject; const APropertyName: string);
    { default }
    class procedure DefaultStartTriggerAnimation(const Target, AInstance: TFmxObject; const APropertyName: string); static;
    class procedure DefaultStartTriggerAnimationWait(const Target: TFmxObject; const AInstance: TFmxObject; const APropertyName: string);
    { animation property }
    /// <summary>Asynchronously animates float type property and don't wait finishing of animation.</summary>
    class procedure AnimateFloat(const Target: TFmxObject; const APropertyName: string; const NewValue: Single; Duration: Single = 0.2;
      AType: TAnimationType = TAnimationType.In; AInterpolation: TInterpolationType = TInterpolationType.Linear);
    /// <summary>Asynchronously animates float type property with start delay and don't wait finishing of animation.</summary>
    class procedure AnimateFloatDelay(const Target: TFmxObject; const APropertyName: string; const NewValue: Single; Duration: Single = 0.2;
      Delay: Single = 0.0; AType: TAnimationType = TAnimationType.In;
      AInterpolation: TInterpolationType = TInterpolationType.Linear);
    /// <summary>Synchronously animates float type property and wait finishing of animation.</summary>
    /// <remarks>Don't use it on the Android. See detains in comment <c>TAnimator</c>.</remarks>
    class procedure AnimateFloatWait(const Target: TFmxObject; const APropertyName: string; const NewValue: Single; Duration: Single = 0.2;
      AType: TAnimationType = TAnimationType.In; AInterpolation: TInterpolationType = TInterpolationType.Linear);
    /// <summary>Asynchronously animates integer type property and don't wait finishing of animation.</summary>
    class procedure AnimateInt(const Target: TFmxObject; const APropertyName: string; const NewValue: Integer; Duration: Single = 0.2;
      AType: TAnimationType = TAnimationType.In; AInterpolation: TInterpolationType = TInterpolationType.Linear);
    /// <summary>Asynchronously animates integer type property with start delay and don't wait finishing of animation.</summary>
    class procedure AnimateIntDelay(const Target: TFmxObject; const APropertyName: string; const NewValue: Integer; Duration: Single = 0.2;
      Delay: Single = 0.0; AType: TAnimationType = TAnimationType.In;
      AInterpolation: TInterpolationType = TInterpolationType.Linear);
    /// <summary>Synchronously animates integer type property and wait finishing of animation.</summary>
    /// <remarks>Don't use it on the Android. See detains in comment <c>TAnimator</c>.</remarks>
    class procedure AnimateIntWait(const Target: TFmxObject; const APropertyName: string; const NewValue: Integer; Duration: Single = 0.2;
      AType: TAnimationType = TAnimationType.In; AInterpolation: TInterpolationType = TInterpolationType.Linear);
    /// <summary>Asynchronously animates color type property and don't wait finishing of animation.</summary>
    class procedure AnimateColor(const Target: TFmxObject; const APropertyName: string; NewValue: TAlphaColor; Duration: Single = 0.2;
      AType: TAnimationType = TAnimationType.In; AInterpolation: TInterpolationType = TInterpolationType.Linear);
    /// <summary>Stops all current animations of specified property of <c>Target</c> object.</summary>
    class procedure StopPropertyAnimation(const Target: TFmxObject; const APropertyName: string);
  end;

{$IFDEF ANDROID}
{ TAniThread }

  TTimerThread = class(TThread)
  private
    FTimerEvent: TNotifyEvent;
    FInterval: Cardinal;
    FEnabled: Boolean;
    FEnabledEvent: TEvent;
    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: Cardinal);
  protected
    procedure Execute; override;
    procedure DoInterval;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    property Interval: Cardinal read FInterval write SetInterval;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property OnTimer: TNotifyEvent read FTimerEvent write FTimerEvent;
  end;

  TThreadTimer = class(TTimer)
  private
    FThread: TTimerThread;
  protected
    procedure SetOnTimer(Value: TNotifyEvent); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetInterval(Value: Cardinal); override;
    procedure UpdateTimer; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure BeforeDestruction; override;
  end;
{$ENDIF ANDROID}

  TAniThread = class({$IFDEF ANDROID}TThreadTimer{$ELSE}TTimer{$ENDIF})
  private
    FAniList: TList<TGameAnimation>;
    FSprList: TList<TGameAnimation>;
    FTime, FDeltaTime: Double;
    FTimerService: IFMXTimerService;
    procedure OneStep;
    procedure DoSyncTimer(Sender: TObject);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure AddAnimation(const Ani: TGameAnimation);
    procedure AddSprite(const Spr: TGameAnimation);
    procedure RemoveAnimation(const Ani: TGameAnimation);

    property AniList: TList<TGameAnimation> read FAniList;
    property SprList: TList<TGameAnimation> read FSprList;
  end;

{ TAnimation }

  /// <summary>Trigger responsible for starting the animation.</summary>
  TGameAnimationTrigger = class
  public type
    TTriggerInfo = record
      Name: string;
      Value: Boolean;
      Prop: TRttiProperty;
    end;
  private
    FTrigger: TTrigger;
    FNames: TDictionary<TTrigger, TTrigger>;
    FRttiInfo: TList<TTriggerInfo>;
    FTargetClass: TClass;
    procedure SetTrigger(const ATrigger: TTrigger);
    procedure ParseTriggerNames(const ATrigger: TTrigger);
    { Rtti info }
    procedure CollectRttiInfo(const ATarget: TObject);
    procedure ClearRttiInfo;
  public
    destructor Destroy; override;
    /// <summary>Whether the trigger condition contains the specified property?</summary>
    function HasProperty(const APropertyName: string): Boolean;
    /// <summary>Is trigger condition is true for specified object <c>ATarget</c>.?</summary>
    function CanExecute(const ATarget: TObject): Boolean;

    /// <summary>Condition for starting animation. Contains list of pairs PropertyName and boolean value separated
    /// by semicolons</summary>
    property Trigger: TTrigger read FTrigger write SetTrigger;
  end;

  TGameAnimation = class(TFmxObject)
  public const
    DefaultAniFrameRate = 60;
  public class var
    AniFrameRate: Integer;
    AniCanvas: TCanvas;
    Matrix: TMatrix;
    Active: Boolean;
    OnPaintMap, OnPaintUI: TNotifyEvent;
  private type
    TTriggerType = (Normal, Inverse);
  private class var
    FAniThread: TAniThread;
  private
    FTickCount: Integer;
    FDuration: Single;
    FDelay: Single;
    FDelayTime: Single;
    FTime: Single;
    FInverse: Boolean;
    FSavedInverse: Boolean;
    FLoop: Boolean;
    FPause: Boolean;
    FRunning: Boolean;
    FAutoRemove: Boolean;
    FOnFinish: TNotifyEvent;
    FOnProcess: TNotifyEvent;
    FInterpolation: TInterpolationType;
    FAnimationType: TAnimationType;
    FEnabled: Boolean;
    FAutoReverse: Boolean;
    { Triggers }
    FTriggers: array [TTriggerType] of TGameAnimationTrigger;
    procedure SetEnabled(const Value: Boolean);
    procedure SetTrigger(const Index: TTriggerType; const Value: TTrigger);
    function GetTrigger(const Index: TTriggerType): TTrigger;
    class procedure Uninitialize;
  protected
    ///<summary>Return normalized CurrentTime value between 0..1 </summary>
    function GetNormalizedTime: Single;
    procedure FirstFrame; virtual;
    procedure ProcessAnimation; virtual; abstract;
    procedure DoProcess; virtual;
    procedure DoFinish; virtual;
    procedure Loaded; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; virtual;
    procedure Stop; virtual;
    procedure StopAtCurrent; virtual;
    /// <summary>Tries to start animation by property name <c>APropertyName</c>. Checks specified properties values
    /// of <c>AInstance</c> in trigger.</summary>
    /// <remarks>If <c>AInstance</c> is not specified, then animation will not start.</remarks>
    procedure StartTrigger(const AInstance: TFmxObject; const APropertyName: string);
    /// <summary>Stops current animation, if specified property <c>APropertyName</c> is in one of triggers.</summary>
    procedure StopTrigger(const AInstance: TFmxObject; const APropertyName: string);
    procedure ProcessTick(const ATime, ADeltaTime: Single);
    property Running: Boolean read FRunning;
    property Pause: Boolean read FPause write FPause;
    property AutoRemove: Boolean read FAutoRemove write FAutoRemove;
    property AnimationType: TAnimationType read FAnimationType write FAnimationType default TAnimationType.In;
    property AutoReverse: Boolean read FAutoReverse write FAutoReverse default False;
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property Delay: Single read FDelay write FDelay;
    property Duration: Single read FDuration write FDuration nodefault;
    property Interpolation: TInterpolationType read FInterpolation write FInterpolation default TInterpolationType.Linear;
    property Inverse: Boolean read FInverse write FInverse default False;
    ///<summary>Normalized CurrentTime value between 0..1 </summary>
    property NormalizedTime: Single read GetNormalizedTime;
    property Loop: Boolean read FLoop write FLoop default False;
    property Trigger: TTrigger index TTriggerType.Normal read GetTrigger write SetTrigger;
    property TriggerInverse: TTrigger index TTriggerType.Inverse read GetTrigger write SetTrigger;
    property CurrentTime: Single read FTime write FTime;
    property OnProcess: TNotifyEvent read FOnProcess write FOnProcess;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    class property AniThread: TAniThread read FAniThread;
  end;

{ TSprteAnimation }
  TSpriteAnimation = class(TGameAnimation)
  public
    destructor Destroy; override;
    
    procedure Start; override;
    procedure Stop; override;  
  end;

{ TCustomPropertyAnimation }

  TCustomPropertyAnimation = class(TGameAnimation)
  private
  protected
    FInstance: TObject;
    FRttiProperty: TRttiProperty;
    FPath, FPropertyName: string;
    FTarget: TFmxObject;
    FTargetName: string;
    procedure SetPropertyName(const AValue: string);
    function FindProperty: Boolean;
    procedure ParentChanged; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    property PropertyName: string read FPropertyName write SetPropertyName;

    procedure Start; override;
    procedure Stop; override;
  end;

{ TFloatAnimation }

  TFloatAnimation = class(TCustomPropertyAnimation)
  private
    FStartFloat: Single;
    FStopFloat: Single;
    FStartFromCurrent: Boolean;
  protected
    procedure ProcessAnimation; override;
    procedure FirstFrame; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AnimationType default TAnimationType.In;
    property AutoReverse default False;
    property Enabled default False;
    property Delay;
    property Duration nodefault;
    property Interpolation default TInterpolationType.Linear;
    property Inverse default False;
    property Loop default False;
    property OnProcess;
    property OnFinish;
    property PropertyName;
    property StartValue: Single read FStartFloat write FStartFloat stored True nodefault;
    property StartFromCurrent: Boolean read FStartFromCurrent write FStartFromCurrent default False;
    property StopValue: Single read FStopFloat write FStopFloat stored True nodefault;
    property Trigger;
    property TriggerInverse;
  end;

{ TIntAnimation }

  TIntAnimation = class(TCustomPropertyAnimation)
  private
    FStartValue: Integer;
    FStopValue: Integer;
    FStartFromCurrent: Boolean;
  protected
    procedure ProcessAnimation; override;
    procedure FirstFrame; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AnimationType default TAnimationType.In;
    property AutoReverse default False;
    property Enabled default False;
    property Delay;
    property Duration nodefault;
    property Interpolation default TInterpolationType.Linear;
    property Inverse default False;
    property Loop default False;
    property OnProcess;
    property OnFinish;
    property PropertyName;
    property StartValue: Integer read FStartValue write FStartValue stored True nodefault;
    property StartFromCurrent: Boolean read FStartFromCurrent write FStartFromCurrent default False;
    property StopValue: Integer read FStopValue write FStopValue stored True nodefault;
    property Trigger;
    property TriggerInverse;
  end;

{ TColorAnimation }

  TColorAnimation = class(TCustomPropertyAnimation)
  private
    FStartColor: TAlphaColor;
    FStopColor: TAlphaColor;
    FStartFromCurrent: Boolean;
  protected
    procedure ProcessAnimation; override;
    procedure FirstFrame; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AnimationType default TAnimationType.In;
    property AutoReverse default False;
    property Enabled default False;
    property Delay;
    property Duration nodefault;
    property Interpolation default TInterpolationType.Linear;
    property Inverse default False;
    property Loop default False;
    property OnProcess;
    property OnFinish;
    property PropertyName;
    property StartValue: TAlphaColor read FStartColor write FStartColor;
    property StartFromCurrent: Boolean read FStartFromCurrent write FStartFromCurrent default False;
    property StopValue: TAlphaColor read FStopColor write FStopColor;
    property Trigger;
    property TriggerInverse;
  end;

{ TGradientAnimation }

  TGradientAnimation = class(TCustomPropertyAnimation)
  private
    FStartGradient: TGradient;
    FStopGradient: TGradient;
    FStartFromCurrent: Boolean;
    procedure SetStartGradient(const Value: TGradient);
    procedure SetStopGradient(const Value: TGradient);
  protected
    procedure ProcessAnimation; override;
    procedure FirstFrame; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AnimationType default TAnimationType.In;
    property AutoReverse default False;
    property Enabled default False;
    property Delay;
    property Duration nodefault;
    property Interpolation default TInterpolationType.Linear;
    property Inverse default False;
    property Loop default False;
    property OnProcess;
    property OnFinish;
    property PropertyName;
    property StartValue: TGradient read FStartGradient write SetStartGradient;
    property StartFromCurrent: Boolean read FStartFromCurrent write FStartFromCurrent default False;
    property StopValue: TGradient read FStopGradient write SetStopGradient;
    property Trigger;
    property TriggerInverse;
  end;

{ TRectAnimation }

  TRectAnimation = class(TCustomPropertyAnimation)
  private
    FStartRect: TBounds;
    FCurrent: TBounds;
    FStopRect: TBounds;
    FStartFromCurrent: Boolean;
    procedure SetStartRect(const Value: TBounds);
    procedure SetStopRect(const Value: TBounds);
  protected
    procedure ProcessAnimation; override;
    procedure FirstFrame; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AnimationType default TAnimationType.In;
    property AutoReverse default False;
    property Enabled default False;
    property Delay;
    property Duration nodefault;
    property Interpolation default TInterpolationType.Linear;
    property Inverse default False;
    property Loop default False;
    property OnProcess;
    property OnFinish;
    property PropertyName;
    property StartValue: TBounds read FStartRect write SetStartRect;
    property StartFromCurrent: Boolean read FStartFromCurrent write FStartFromCurrent default False;
    property StopValue: TBounds read FStopRect write SetStopRect;
    property Trigger;
    property TriggerInverse;
  end;

{ TBitmapAnimation }

  TBitmapAnimation = class(TCustomPropertyAnimation)
  private
    FStartBitmap: TBitmap;
    FStopBitmap: TBitmap;
    procedure SetStartBitmap(Value: TBitmap);
    procedure SetStopBitmap(Value: TBitmap);
  protected
    procedure ProcessAnimation; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AnimationType default TAnimationType.In;
    property AutoReverse default False;
    property Enabled default False;
    property Delay;
    property Duration nodefault;
    property Interpolation default TInterpolationType.Linear;
    property Inverse default False;
    property Loop default False;
    property OnProcess;
    property OnFinish;
    property PropertyName;
    property StartValue: TBitmap read FStartBitmap write SetStartBitmap;
    property StopValue: TBitmap read FStopBitmap write SetStopBitmap;
    property Trigger;
    property TriggerInverse;
  end;

{ TBitmapListAnimation }

  TBitmapListAnimation = class(TCustomPropertyAnimation)
  private type
    TAnimationBitmap = class(TBitmap)
    private
      [Weak] FAnimation: TBitmapListAnimation;
    protected
      procedure ReadStyleLookup(Reader: TReader); override;
    end;
  private
    FAnimationCount: Integer;
    FAnimationBitmap: TBitmap;
    FLastAnimationStep: Integer;
    FAnimationRowCount: Integer;
    FAnimationLookup: string;
    procedure SetAnimationBitmap(Value: TBitmap);
    procedure SetAnimationRowCount(const Value: Integer);
    procedure SetAnimationLookup(const Value: string);
    procedure RefreshBitmap(const WorkBitmap: TBitmap = nil);
  protected
    procedure ProcessAnimation; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AnimationBitmap: TBitmap read FAnimationBitmap write SetAnimationBitmap;
    property AnimationLookup: string read FAnimationLookup write SetAnimationLookup;
    property AnimationCount: Integer read FAnimationCount write FAnimationCount;
    property AnimationRowCount: Integer read FAnimationRowCount write SetAnimationRowCount default 1;
    property AnimationType default TAnimationType.In;
    property AutoReverse default False;
    property Enabled default False;
    property Delay;
    property Duration nodefault;
    property Interpolation default TInterpolationType.Linear;
    property Inverse default False;
    property Loop default False;
    property OnProcess;
    property OnFinish;
    property PropertyName;
    property Trigger;
    property TriggerInverse;
  end;

{ Key Animations }

{ TKey }

  TKey = class(TCollectionItem)
  private
    FKey: Single;
    procedure SetKey(const Value: Single);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Key: Single read FKey write SetKey;
  end;

{ TKeys }

  TKeys = class(TCollection)
  public
    function FindKeys(const Time: Single; var Key1, Key2: TKey): Boolean;
  end;

{ TColorKey }

  TColorKey = class(TKey)
  private
    FValue: TAlphaColor;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Value: TAlphaColor read FValue write FValue;
  end;

{ TColorKeyAnimation }

  TColorKeyAnimation = class(TCustomPropertyAnimation)
  private
    FKeys: TKeys;
    FStartFromCurrent: Boolean;
    procedure SetKeys(const Value: TKeys);
  protected
    procedure ProcessAnimation; override;
    procedure FirstFrame; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AnimationType default TAnimationType.In;
    property AutoReverse default False;
    property Enabled default False;
    property Delay;
    property Duration nodefault;
    property Interpolation default TInterpolationType.Linear;
    property Inverse default False;
    property Keys: TKeys read FKeys write SetKeys;
    property Loop default False;
    property OnProcess;
    property OnFinish;
    property PropertyName;
    property StartFromCurrent: Boolean read FStartFromCurrent write FStartFromCurrent;
    property Trigger;
    property TriggerInverse;
  end;

{ TFloatKey }

  TFloatKey = class(TKey)
  private
    FValue: Single;
  published
    property Value: Single read FValue write FValue;
  end;

{ TFloatKeyAnimation }

  TFloatKeyAnimation = class(TCustomPropertyAnimation)
  private
    FKeys: TKeys;
    FStartFromCurrent: Boolean;
    procedure SetKeys(const Value: TKeys);
  protected
    procedure ProcessAnimation; override;
    procedure FirstFrame; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AnimationType default TAnimationType.In;
    property AutoReverse default False;
    property Enabled default False;
    property Delay;
    property Duration nodefault;
    property Interpolation default TInterpolationType.Linear;
    property Inverse default False;
    property Keys: TKeys read FKeys write SetKeys;
    property Loop default False;
    property OnProcess;
    property OnFinish;
    property PropertyName;
    property StartFromCurrent: Boolean read FStartFromCurrent write FStartFromCurrent;
    property Trigger;
    property TriggerInverse;
  end;

function InterpolateLinear(t, B, C, D: Single): Single;
function InterpolateSine(t, B, C, D: Single; AType: TAnimationType): Single;
function InterpolateQuint(t, B, C, D: Single; AType: TAnimationType): Single;
function InterpolateQuart(t, B, C, D: Single; AType: TAnimationType): Single;
function InterpolateQuad(t, B, C, D: Single; AType: TAnimationType): Single;
function InterpolateExpo(t, B, C, D: Single; AType: TAnimationType): Single;
function InterpolateElastic(t, B, C, D, A, P: Single; AType: TAnimationType): Single;
function InterpolateCubic(t, B, C, D: Single; AType: TAnimationType): Single;
function InterpolateCirc(t, B, C, D: Single; AType: TAnimationType): Single;
function InterpolateBounce(t, B, C, D: Single; AType: TAnimationType): Single;
function InterpolateBack(t, B, C, D, S: Single; AType: TAnimationType): Single;

implementation

uses
  System.Types, System.Math, System.SysUtils, System.StrUtils, System.TypInfo,
  {$IFDEF MACOS}Macapi.CoreFoundation, {$ENDIF} FMX.Platform, FMX.Forms, FMX.Utils, FMX.MultiResBitmap;

var
  PropAnimationList: TFmxObject;

function InterpolateBack(t, B, C, D, S: Single; AType: TAnimationType): Single;
begin
  case AType of
    TAnimationType.In:
      begin
        if S = 0 then
          S := 1.70158;
        t := t / D;
        Result := C * t * t * ((S + 1) * t - S) + B;
      end;
    TAnimationType.Out:
      begin
        if S = 0 then
          S := 1.70158;
        t := t / D - 1;
        Result := C * (t * t * ((S + 1) * t + S) + 1) + B;
      end;
    TAnimationType.InOut:
      begin
        if S = 0 then
          S := 1.70158;
        t := t / (D / 2);
        if t < 1 then
        begin
          S := S * 1.525;
          Result := C / 2 * (t * t * ((S + 1) * t - S)) + B;
        end
        else
        begin
          t := t - 2;
          S := S * 1.525;
          Result := C / 2 * (t * t * ((S + 1) * t + S) + 2) + B;
        end;
      end;
  else
    Result := 0;
  end;
end;

function InterpolateBounce(t, B, C, D: Single; AType: TAnimationType): Single;
  function _EaseOut(t, B, C, D: Single): Single;
  begin
    t := t / D;
    if t < 1 / 2.75 then
    begin
      Result := C * (7.5625 * t * t) + B;
    end
    else if t < 2 / 2.72 then
    begin
      t := t - (1.5 / 2.75);
      Result := C * (7.5625 * t * t + 0.75) + B;
    end
    else if t < 2.5 / 2.75 then
    begin
      t := t - (2.25 / 2.75);
      Result := C * (7.5625 * t * t + 0.9375) + B;
    end
    else
    begin
      t := t - (2.625 / 2.75);
      Result := C * (7.5625 * t * t + 0.984375) + B;
    end;
  end;
  function _EaseIn(t, B, C, D: Single): Single;
  begin
    Result := C - _EaseOut(D - t, 0, C, D) + B;
  end;

begin
  case AType of
    TAnimationType.In:
      begin
        Result := _EaseIn(t, B, C, D);
      end;
    TAnimationType.Out:
      begin
        Result := _EaseOut(t, B, C, D);
      end;
    TAnimationType.InOut:
      begin
        if t < D / 2 then
          Result := _EaseIn(t * 2, 0, C, D) * 0.5 + B
        else
          Result := _EaseOut(t * 2 - D, 0, C, D) * 0.5 + C * 0.5 + B;
      end;
  else
    Result := 0;
  end;
end;

function InterpolateCirc(t, B, C, D: Single; AType: TAnimationType): Single;
begin
  case AType of
    TAnimationType.In:
      begin
        t := t / D;
        Result := -C * (Sqrt(1 - t * t) - 1) + B;
      end;
    TAnimationType.Out:
      begin
        t := t / D - 1;
        Result := C * Sqrt(1 - t * t) + B;
      end;
    TAnimationType.InOut:
      begin
        t := t / (D / 2);
        if t < 1 then
          Result := -C / 2 * (Sqrt(1 - t * t) - 1) + B
        else
        begin
          t := t - 2;
          Result := C / 2 * (Sqrt(1 - t * t) + 1) + B;
        end;
      end;
  else
    Result := 0;
  end;
end;

function InterpolateCubic(t, B, C, D: Single; AType: TAnimationType): Single;
begin
  case AType of
    TAnimationType.In:
      begin
        t := t / D;
        Result := C * t * t * t + B;
      end;
    TAnimationType.Out:
      begin
        t := t / D - 1;
        Result := C * (t * t * t + 1) + B;
      end;
    TAnimationType.InOut:
      begin
        t := t / (D / 2);
        if t < 1 then
          Result := C / 2 * t * t * t + B
        else
        begin
          t := t - 2;
          Result := C / 2 * (t * t * t + 2) + B;
        end;
      end;
  else
    Result := 0;
  end;
end;

function InterpolateElastic(t, B, C, D, A, P: Single; AType: TAnimationType): Single;
var
  S: Single;
begin
  case AType of
    TAnimationType.In:
      begin
        if t = 0 then
        begin
          Result := B;
          Exit;
        end;
        t := t / D;
        if t = 1 then
        begin
          Result := B + C;
          Exit;
        end;
        if P = 0 then
          P := D * 0.3;
        if (A = 0) or (A < Abs(C)) then
        begin
          A := C;
          S := P / 4;
        end
        else
        begin
          S := P / (2 * Pi) * ArcSin((C / A));
        end;
        t := t - 1;
        Result := -(A * Power(2, (10 * t)) * Sin((t * D - S) * (2 * Pi) / P)) + B;
      end;
    TAnimationType.Out:
      begin
        if t = 0 then
        begin
          Result := B;
          Exit;
        end;
        t := t / D;
        if t = 1 then
        begin
          Result := B + C;
          Exit;
        end;
        if P = 0 then
          P := D * 0.3;
        if (A = 0) or (A < Abs(C)) then
        begin
          A := C;
          S := P / 4;
        end
        else
        begin
          S := P / (2 * Pi) * ArcSin((C / A));
        end;
        Result := A * Power(2, (-10 * t)) * Sin((t * D - S) * (2 * Pi) / P) + C + B;
      end;
    TAnimationType.InOut:
      begin
        if t = 0 then
        begin
          Result := B;
          Exit;
        end;
        t := t / (D / 2);
        if t = 2 then
        begin
          Result := B + C;
          Exit;
        end;
        if P = 0 then
          P := D * (0.3 * 1.5);
        if (A = 0) or (A < Abs(C)) then
        begin
          A := C;
          S := P / 4;
        end
        else
        begin
          S := P / (2 * Pi) * ArcSin((C / A));
        end;

        if t < 1 then
        begin
          t := t - 1;
          Result := -0.5 * (A * Power(2, (10 * t)) * Sin((t * D - S) * (2 * Pi) / P)) + B;
        end
        else
        begin
          t := t - 1;
          Result := A * Power(2, (-10 * t)) * Sin((t * D - S) * (2 * Pi) / P) * 0.5 + C + B;
        end;
      end;
  else
    Result := 0;
  end;
end;

function InterpolateExpo(t, B, C, D: Single; AType: TAnimationType): Single;
begin
  case AType of
    TAnimationType.In:
      begin
        If t = 0 Then
          Result := B
        else
          Result := C * Power(2, (10 * (t / D - 1))) + B;
      end;
    TAnimationType.Out:
      begin
        If t = D then
          Result := B + C
        else
          Result := C * (-Power(2, (-10 * t / D)) + 1) + B;
      end;
    TAnimationType.InOut:
      begin
        if t = 0 then
        begin
          Result := B;
          Exit;
        end;
        if t = D then
        begin
          Result := B + C;
          Exit;
        end;
        t := t / (D / 2);
        if t < 1 then
          Result := C / 2 * Power(2, (10 * (t - 1))) + B
        else
        begin
          t := t - 1;
          Result := C / 2 * (-Power(2, (-10 * t)) + 2) + B;
        end;
      end;
  else
    Result := 0;
  end;
end;

function InterpolateLinear(t, B, C, D: Single): Single;
begin
  Result := C * t / D + B;
end;

function InterpolateQuad(t, B, C, D: Single; AType: TAnimationType): Single;
begin
  case AType of
    TAnimationType.In:
      begin
        t := t / D;
        Result := C * t * t + B;
      end;
    TAnimationType.Out:
      begin
        t := t / D;
        Result := -C * t * (t - 2) + B;
      end;
    TAnimationType.InOut:
      begin
        t := t / (D / 2);

        if t < 1 then
          Result := C / 2 * t * t + B
        else
        begin
          t := t - 1;
          Result := -C / 2 * (t * (t - 2) - 1) + B;
        end;
      end;
  else
    Result := 0;
  end;
end;

function InterpolateQuart(t, B, C, D: Single; AType: TAnimationType): Single;
begin
  case AType of
    TAnimationType.In:
      begin
        t := t / D;
        Result := C * t * t * t * t + B;
      end;
    TAnimationType.Out:
      begin
        t := t / D - 1;
        Result := -C * (t * t * t * t - 1) + B;
      end;
    TAnimationType.InOut:
      begin
        t := t / (D / 2);
        if t < 1 then
          Result := C / 2 * t * t * t * t + B
        else
        begin
          t := t - 2;
          Result := -C / 2 * (t * t * t * t - 2) + B;
        end;
      end;
  else
    Result := 0;
  end;
end;

function InterpolateQuint(t, B, C, D: Single; AType: TAnimationType): Single;
begin
  case AType of
    TAnimationType.In:
      begin
        t := t / D;
        Result := C * t * t * t * t * t + B;
      end;
    TAnimationType.Out:
      begin
        t := t / D - 1;
        Result := C * (t * t * t * t * t + 1) + B;
      end;
    TAnimationType.InOut:
      begin
        t := t / (D / 2);
        if t < 1 then
          Result := C / 2 * t * t * t * t * t + B
        else
        begin
          t := t - 2;
          Result := C / 2 * (t * t * t * t * t + 2) + B;
        end;
      end;
  else
    Result := 0;
  end;
end;

function InterpolateSine(t, B, C, D: Single; AType: TAnimationType): Single;
begin
  case AType of
    TAnimationType.In:
      begin
        Result := -C * Cos(t / D * (Pi / 2)) + C + B;
      end;
    TAnimationType.Out:
      begin
        Result := C * Sin(t / D * (Pi / 2)) + B;
      end;
    TAnimationType.InOut:
      begin
        Result := -C / 2 * (Cos(Pi * t / D) - 1) + B;
      end;
  else
    Result := 0;
  end;
end;

{ TAnimator.TAnimationDestroyer }

constructor TGameAnimator.TAnimationDestroyer.Create;
begin
  FAnimations := TList<Pointer>.Create
end;

destructor TGameAnimator.TAnimationDestroyer.Destroy;
var
  p: TObject;
begin
  for p in FAnimations do
    p.DisposeOf;
  FreeAndNil(FAnimations);
  inherited;
end;

procedure TGameAnimator.TAnimationDestroyer.DoAniFinished(Sender: TObject);
begin
  TThread.ForceQueue(nil, procedure begin
    if FAnimations.Contains(Sender) then
    begin
      TGameAnimation(Sender).DisposeOf;
      FAnimations.Remove(Sender);
    end;
  end);
end;

procedure TGameAnimator.TAnimationDestroyer.FreeNotification(AObject: TObject);
begin
  FAnimations.Remove(AObject);
end;

procedure TGameAnimator.TAnimationDestroyer.RegisterAnimation(const AAnimation: TGameAnimation);
begin
  FAnimations.Add(AAnimation);
  AAnimation.OnFinish := DoAniFinished;
  AAnimation.AddFreeNotify(Self);
end;

{ TAnimator }

class procedure TGameAnimator.StartAnimation(const Target: TFmxObject; const AName: string);
var
  I: Integer;
  E: TGameAnimation;
begin
  I := 0;
  while (Target.Children <> nil) and (I < Target.Children.Count) do
  begin
    if Target.Children[I] is TGameAnimation then
      if CompareText(TGameAnimation(Target.Children[I]).Name, AName) = 0 then
      begin
        E := TGameAnimation(Target.Children[I]);
        E.Start;
      end;
    Inc(I);
  end;
end;

class procedure TGameAnimator.StopAnimation(const Target: TFmxObject; const AName: string);
var
  I: Integer;
  E: TGameAnimation;
  sname: string;
begin
  sname := Target.Name + '.' + AName;
    for I := TGameAnimation.AniThread.AniList.Count - 1 downto 0 do
    begin
      E := TGameAnimation.AniThread.AniList[i];
      if E is TCustomPropertyAnimation then
        if CompareText(TCustomPropertyAnimation(E).FTargetName, sname) = 0 then
        begin
//          E := TGameAnimation(Target.Children[I]);
          E.Stop;
        end;
    end;
end;

class procedure TGameAnimator.StartTriggerAnimation(const Target: TFmxObject; const AInstance: TFmxObject; const APropertyName: string);
var
  Animatable: ITriggerAnimation;
begin
  StopTriggerAnimation(Target, AInstance, APropertyName);
  if Supports(Target, ITriggerAnimation, Animatable) then
    Animatable.StartTriggerAnimation(AInstance, APropertyName)
  else
    DefaultStartTriggerAnimation(Target, AInstance, APropertyName);
end;

class procedure TGameAnimator.DefaultStartTriggerAnimation(const Target: TFmxObject; const AInstance: TFmxObject; const APropertyName: string);
var
  I: Integer;
  Control: IControl;
begin
  if (Target <> nil) and (Target.Children <> nil) then
    for I := 0 to Target.Children.Count - 1 do
    begin
      if Target.Children[I] is TGameAnimation then
        TGameAnimation(Target.Children[I]).StartTrigger(AInstance, APropertyName);
      if Supports(Target.Children[I], IControl, Control) and Control.Locked and not Control.HitTest then
        StartTriggerAnimation(Target.Children[I], AInstance, APropertyName);
    end;
end;

class procedure TGameAnimator.StartTriggerAnimationWait(const Target: TFmxObject; const AInstance: TFmxObject; const APropertyName: string);
var
  Animatable: ITriggerAnimation;
begin
  StopTriggerAnimation(Target, AInstance, APropertyName);
  if Supports(Target, ITriggerAnimation, Animatable) then
    Animatable.StartTriggerAnimationWait(AInstance, APropertyName)
  else
    DefaultStartTriggerAnimationWait(Target, AInstance, APropertyName);
end;

class procedure TGameAnimator.DefaultStartTriggerAnimationWait(const Target, AInstance: TFmxObject; const APropertyName: string);
var
  I: Integer;
  Control: IControl;
begin
  if Target.Children <> nil then
    for I := 0 to Target.Children.Count - 1 do
    begin
      if Target.Children[I] is TGameAnimation then
      begin
        TGameAnimation(Target.Children[I]).StartTrigger(AInstance, APropertyName);
        while TGameAnimation(Target.Children[I]).Running do
        begin
          Application.ProcessMessages;
          Sleep(0);
        end;
      end;
      if Supports(Target.Children[I], IControl, Control) and Control.Locked and not Control.HitTest then
        StartTriggerAnimationWait(Target.Children[I], AInstance, APropertyName);
    end;
end;

class procedure TGameAnimator.StopTriggerAnimation(const Target: TFmxObject; const AInstance: TFmxObject; const APropertyName: string);
var
  Item: TFmxObject;
  Control: IControl;
begin
  if Target.Children <> nil then
    for Item in Target.Children do
    begin
      if TFmxObject(Item) is TGameAnimation then
        TGameAnimation(Item).StopTrigger(AInstance, APropertyName);
      if Supports(Item, IControl, Control) and Control.Locked and not Control.HitTest then
        StopTriggerAnimation(Item, AInstance, APropertyName);
    end;
end;

{ Property animation }

class procedure TGameAnimator.AnimateColor(const Target: TFmxObject; const APropertyName: string; NewValue: TAlphaColor;
  Duration: Single = 0.2; AType: TAnimationType = TAnimationType.In;
  AInterpolation: TInterpolationType = TInterpolationType.Linear);
var
  Animation: TColorAnimation;
begin
  StopPropertyAnimation(Target, APropertyName);

  CreateDestroyer;

  Animation := TColorAnimation.Create(Target);
  Animation.FTarget := Target;
  Animation.AnimationType := AType;
  Animation.Interpolation := AInterpolation;
  Animation.Duration := Duration;
  Animation.PropertyName := APropertyName;
  Animation.StartFromCurrent := True;
  Animation.StopValue := NewValue;
  FDestroyer.RegisterAnimation(Animation);
  Animation.Start;
end;

class procedure TGameAnimator.AnimateFloat(const Target: TFmxObject; const APropertyName: string; const NewValue: Single;
  Duration: Single = 0.2; AType: TAnimationType = TAnimationType.In;
  AInterpolation: TInterpolationType = TInterpolationType.Linear);
var
  Animation: TFloatAnimation;
begin
  StopPropertyAnimation(Target, APropertyName);

  CreateDestroyer;

  Animation := TFloatAnimation.Create(nil);
  Animation.FTarget := Target;
  Animation.AnimationType := AType;
  Animation.Interpolation := AInterpolation;
  Animation.Duration := Duration;
  Animation.PropertyName := APropertyName;
  Animation.StartFromCurrent := True;
  Animation.StopValue := NewValue;
  FDestroyer.RegisterAnimation(Animation);
  Animation.Start;
end;

class procedure TGameAnimator.AnimateFloatDelay(const Target: TFmxObject; const APropertyName: string; const NewValue: Single;
  Duration: Single = 0.2; Delay: Single = 0.0; AType: TAnimationType = TAnimationType.In;
   AInterpolation: TInterpolationType = TInterpolationType.Linear);
var
  Animation: TFloatAnimation;
begin
  CreateDestroyer;

  Animation := TFloatAnimation.Create(nil);
  Animation.FTarget := Target;
  Animation.AnimationType := AType;
  Animation.Interpolation := AInterpolation;
  Animation.Delay := Delay;
  Animation.Duration := Duration;
  Animation.PropertyName := APropertyName;
  Animation.StartFromCurrent := True;
  Animation.StopValue := NewValue;
  FDestroyer.RegisterAnimation(Animation);
  Animation.Start;
end;

class procedure TGameAnimator.AnimateFloatWait(const Target: TFmxObject; const APropertyName: string; const NewValue: Single;
  Duration: Single = 0.2; AType: TAnimationType = TAnimationType.In;
  AInterpolation: TInterpolationType = TInterpolationType.Linear);
var
  Animation: TFloatAnimation;
begin
  StopPropertyAnimation(Target, APropertyName);

  Animation := TFloatAnimation.Create(nil);
  try
    Animation.FTarget := Target;
    Animation.AnimationType := AType;
    Animation.Interpolation := AInterpolation;
    Animation.Duration := Duration;
    Animation.PropertyName := APropertyName;
    Animation.StartFromCurrent := True;
    Animation.StopValue := NewValue;
    Animation.Start;
    while Animation.FRunning do
    begin
      Application.ProcessMessages;
      Sleep(0);
    end;
  finally
    Animation.DisposeOf;
  end;
end;

class procedure TGameAnimator.AnimateInt(const Target: TFmxObject; const APropertyName: string; const NewValue: Integer;
  Duration: Single = 0.2; AType: TAnimationType = TAnimationType.In;
  AInterpolation: TInterpolationType = TInterpolationType.Linear);
var
  Animation: TIntAnimation;
begin
  CreateDestroyer;

  StopPropertyAnimation(Target, APropertyName);

  Animation := TIntAnimation.Create(nil);
  Animation.FTarget := Target;
  Animation.AnimationType := AType;
  Animation.Interpolation := AInterpolation;

  Animation.Duration := Duration;
  Animation.PropertyName := APropertyName;
  Animation.StartFromCurrent := True;
  Animation.StopValue := NewValue;
  FDestroyer.RegisterAnimation(Animation);
  Animation.Start;
end;

class procedure TGameAnimator.AnimateIntDelay(const Target: TFmxObject;
  const APropertyName: string; const NewValue: Integer; Duration, Delay: Single;
  AType: TAnimationType; AInterpolation: TInterpolationType);
var
  Animation: TIntAnimation;
begin
  CreateDestroyer;

  StopPropertyAnimation(Target, APropertyName);

  Animation := TIntAnimation.Create(nil);
  Animation.FTarget := Target;
  Animation.AnimationType := AType;
  Animation.Interpolation := AInterpolation;
  Animation.Delay := Delay;
  Animation.Duration := Duration;
  Animation.PropertyName := APropertyName;
  Animation.StartFromCurrent := True;
  Animation.StopValue := NewValue;
  FDestroyer.RegisterAnimation(Animation);
  Animation.Start;
end;

class procedure TGameAnimator.AnimateIntWait(const Target: TFmxObject; const APropertyName: string; const NewValue: Integer;
  Duration: Single = 0.2; AType: TAnimationType = TAnimationType.In;
  AInterpolation: TInterpolationType = TInterpolationType.Linear);
var
  Animation: TIntAnimation;
begin
  StopPropertyAnimation(Target, APropertyName);

  Animation := TIntAnimation.Create(nil);
  try
    Animation.FTarget := Target;
    Animation.AnimationType := AType;
    Animation.Interpolation := AInterpolation;
    Animation.Duration := Duration;
    Animation.PropertyName := APropertyName;
    Animation.StartFromCurrent := True;
    Animation.StopValue := NewValue;
    Animation.Start;
    while Animation.FRunning do
    begin
      Application.ProcessMessages;
      Sleep(0);
    end;
  finally
    Animation.DisposeOf;
  end;
end;

class procedure TGameAnimator.StopPropertyAnimation(const Target: TFmxObject; const APropertyName: string);
var
  I: Integer;
begin
  I := Target.ChildrenCount - 1;
  while I >= 0 do
  begin
    if (Target.Children[I] is TCustomPropertyAnimation) and
       (CompareText(TCustomPropertyAnimation(Target.Children[I]).PropertyName, APropertyName) = 0) then
      TFloatAnimation(Target.Children[I]).Stop;
    if I > Target.ChildrenCount then
      I := Target.ChildrenCount;
    Dec(I);
  end;
end;

class procedure TGameAnimator.CreateDestroyer;
begin
  if FDestroyer = nil then
    FDestroyer := TAnimationDestroyer.Create;
end;

class procedure TGameAnimator.Uninitialize;
begin
  FDestroyer := nil;
end;

constructor TAniThread.Create;
begin
  inherited Create(nil);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, FTimerService) then
    raise EUnsupportedPlatformService.Create('IFMXTimerService');
  TGameAnimation.AniFrameRate := EnsureRange(TGameAnimation.AniFrameRate, 5, 100);
  Interval := Trunc(1000 / TGameAnimation.AniFrameRate / 10) * 10;
  if (Interval <= 0) then
    Interval := 1;

  OnTimer := DoSyncTimer;
  FAniList := TList<TGameAnimation>.Create;
  FSprList := TList<TGameAnimation>.Create;
  FTime := FTimerService.GetTick;

  Enabled := False;
end;

destructor TAniThread.Destroy;
begin
  FreeAndNil(FAniList);
  FreeAndNil(FSprList);
  FTimerService := nil;
  inherited;
end;

procedure TAniThread.AddAnimation(const Ani: TGameAnimation);
begin
  if FAniList.IndexOf(Ani) < 0 then
    FAniList.Insert(0, Ani);
  if not Enabled then
  begin
    FTime := FTimerService.GetTick;
    Enabled := True;
  end;
end;

procedure TAniThread.AddSprite(const Spr: TGameAnimation);
begin
  if FSprList.IndexOf(Spr) < 0 then
    FSprList.Insert(0, Spr);
  if not Enabled then
  begin
    FTime := FTimerService.GetTick;
    Enabled := True;
  end;
end;

procedure TAniThread.RemoveAnimation(const Ani: TGameAnimation);
begin
  FAniList.Remove(Ani);
  Enabled := (FAniList.Count > 0) or (FSprList.Count > 0);
end;

procedure TAniThread.DoSyncTimer(Sender: TObject);
begin
  OneStep;
  if TGameAnimation.AniFrameRate < 5 then
    TGameAnimation.AniFrameRate := 5;
  Interval := Trunc(1000 / TGameAnimation.AniFrameRate / 10) * 10;
  if (Interval <= 0) then Interval := 1;
end;

procedure TAniThread.OneStep;
var
  I: Integer;
  NewTime: Double;
  [unsafe] Ani: TGameAnimation;
begin
  NewTime := FTimerService.GetTick;
  FDeltaTime := NewTime - FTime;
  FTime := NewTime;
  if FDeltaTime <= 0 then
    Exit;
  if not TGameAnimation.Active then
    Exit;
  if FSprList.Count > 0 then
  begin
    if TGameAnimation.AniCanvas.BeginScene() then
    try
      TGameAnimation.AniCanvas.Clear(0);
      TGameAnimation.AniCanvas.SetMatrix(TGameAnimation.Matrix);
      if Assigned(TGameAnimation.OnPaintMap) then
        TGameAnimation.OnPaintMap(nil);
      I := FSprList.Count - 1;
      while I >= 0 do
      begin
        Ani := FSprList[I];
        if Ani.FRunning then
          Ani.ProcessTick(FTime, FDeltaTime);
        Dec(I);
        if I >= FSprList.Count then
          I := FSprList.Count - 1;
      end;

      if Assigned(TGameAnimation.OnPaintUI) then
        TGameAnimation.OnPaintUI(nil);
    finally
      TGameAnimation.AniCanvas.EndScene;
    end;
  end;

  if FAniList.Count > 0 then
  begin
    I := FAniList.Count - 1;
    while I >= 0 do
    begin
      Ani := FAniList[I];
      if Ani.FRunning then
      begin
        if (Ani.StyleName <> '') and
          (CompareText(Ani.StyleName, 'caret') = 0) then
        begin
          Ani.Tag := Ani.Tag + 1;
          if Ani.Tag mod 12 = 0 then
            Ani.ProcessTick(FTime, FDeltaTime);
        end
        else
          Ani.ProcessTick(FTime, FDeltaTime);
      end;
      Dec(I);
      if I >= FAniList.Count then
        I := FAniList.Count - 1;
    end;
  end;
end;

{ TAnimationTrigger }

function TGameAnimationTrigger.CanExecute(const ATarget: TObject): Boolean;
var
  I: Integer;
  TriggerInfo: TTriggerInfo;
  PropertyValue: TValue;
begin
  if ATarget = nil then
    Exit(False);

  if not ATarget.InheritsFrom(FTargetClass) then
    ClearRttiInfo;

  if FRttiInfo = nil then
    CollectRttiInfo(ATarget);

  if (FRttiInfo = nil) or not ATarget.InheritsFrom(FTargetClass) then
    Exit(False);

  Result := False;
  for I := 0 to FRttiInfo.Count - 1 do
  begin
    TriggerInfo := FRttiInfo[I];
    PropertyValue := TriggerInfo.Prop.GetValue(ATarget);
    Result := PropertyValue.AsBoolean = TriggerInfo.Value;
    if not Result then
      Break;
  end;
end;

procedure TGameAnimationTrigger.ClearRttiInfo;
begin
  FreeAndNil(FRttiInfo);
  FTargetClass := nil;
end;

procedure TGameAnimationTrigger.CollectRttiInfo(const ATarget: TObject);
var
  T: TRttiType;

  procedure Parse(var ATriggerList: TList<TTriggerInfo>; const ATrigger: string);
  var
    Prop: TRttiProperty;
    Line: string;
    Setter: string;
    LName: string;
    Value: string;
    TriggerRec: TTriggerInfo;
  begin
    Line := ATrigger;
    Setter := GetToken(Line, ';');
    while not Setter.IsEmpty do
    begin
      LName := GetToken(Setter, '=');
      Value := Setter;
      Prop := T.GetProperty(LName);
      if (Prop <> nil) and IsBoolType(Prop.PropertyType.Handle) then
      begin
        TriggerRec.Name := LName;
        TriggerRec.Prop := Prop;
        TriggerRec.Value := StrToBoolDef(Value, True);
        ATriggerList.Add(TriggerRec);
      end
      else
      begin
        FreeAndNil(ATriggerList);
        Break;
      end;
      Setter := GetToken(Line, ';');
    end;
  end;

begin
  if FRttiInfo <> nil then
    Exit;

  T := SharedContext.GetType(ATarget.ClassInfo);
  if T = nil then
    Exit;

  FRttiInfo := TList<TTriggerInfo>.Create;
  Parse(FRttiInfo, FTrigger);

  FTargetClass := ATarget.ClassType;
end;

destructor TGameAnimationTrigger.Destroy;
begin
  FreeAndNil(FRttiInfo);
  FreeAndNil(FNames);
  inherited;
end;

function TGameAnimationTrigger.HasProperty(const APropertyName: string): Boolean;
begin
  Result := (FNames <> nil) and FNames.ContainsKey(APropertyName.ToLower);
end;

procedure TGameAnimationTrigger.ParseTriggerNames(const ATrigger: TTrigger);

  procedure Parse(const ATrigger: string; const ATriggerList: TDictionary<TTrigger, TTrigger>);
  var
    LLine: string;
    LTrigger: string;
    LName: string;
  begin
    LLine := ATrigger;
    LTrigger := GetToken(LLine, ';');
    while not LTrigger.IsEmpty do
    begin
      LName := GetToken(LTrigger, '=');
      ATriggerList.Add(LName.ToLower, LName);
      LTrigger := GetToken(LLine, ';');
    end;
  end;

begin
  if ATrigger = '' then
    FreeAndNil(FNames)
  else
  begin
    FNames := TDictionary<TTrigger, TTrigger>.Create;
    Parse(ATrigger, FNames);
  end;
end;

procedure TGameAnimationTrigger.SetTrigger(const ATrigger: TTrigger);
begin
  if FTrigger <> ATrigger then
  begin
    FTrigger := ATrigger;
    ParseTriggerNames(FTrigger);
    ClearRttiInfo;
  end;
end;

{ TAnimation }

procedure TGameAnimation.AssignTo(Dest: TPersistent);
var
  DestAnimation: TGameAnimation;
begin
  if Dest is TGameAnimation then
  begin
    DestAnimation := TGameAnimation(Dest);
    DestAnimation.AnimationType := AnimationType;
    DestAnimation.AutoReverse := AutoReverse;
    DestAnimation.Duration := Duration;
    DestAnimation.Delay := Delay;
    DestAnimation.Interpolation := Interpolation;
    DestAnimation.Inverse := Inverse;
    DestAnimation.Loop := Loop;
    DestAnimation.Trigger := Trigger;
    DestAnimation.TriggerInverse := TriggerInverse;
    DestAnimation.Enabled := Enabled;
  end
  else
    inherited;
end;

constructor TGameAnimation.Create(AOwner: TComponent);
var
  TriggerType: TTriggerType;
begin
  inherited;
  FEnabled := False;
  FAutoRemove := True;
  Duration := 0.2;
  for TriggerType := Low(TTriggerType) to High(TTriggerType) do
    FTriggers[TriggerType] := TGameAnimationTrigger.Create;
end;

destructor TGameAnimation.Destroy;
var
  TriggerType: TTriggerType;
begin
  for TriggerType := Low(TTriggerType) to High(TTriggerType) do
    FreeAndNil(FTriggers[TriggerType]);
  if AniThread <> nil then
  begin
    if Self is TSpriteAnimation then
      TAniThread(AniThread).FSprList.Remove(Self)
    else
      TAniThread(AniThread).FAniList.Remove(Self);
  end;
  inherited;
end;

procedure TGameAnimation.FirstFrame;
begin
end;

procedure TGameAnimation.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) and Enabled then
    Start;
end;

procedure TGameAnimation.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if [csDesigning, csReading, csLoading] * ComponentState = [] then
    begin
      if FEnabled then
        Start
      else
        Stop;
    end;
  end;
end;

procedure TGameAnimation.SetTrigger(const Index: TTriggerType; const Value: TTrigger);
begin
  FTriggers[Index].Trigger := Value;
end;

function TGameAnimation.GetNormalizedTime: Single;
begin
  Result := 0;
  if (FDuration > 0) and (FDelayTime <= 0) then
  begin
    case FInterpolation of
      TInterpolationType.Linear:
        Result := InterpolateLinear(FTime, 0, 1, FDuration);
      TInterpolationType.Quadratic:
        Result := InterpolateQuad(FTime, 0, 1, FDuration, FAnimationType);
      TInterpolationType.Cubic:
        Result := InterpolateCubic(FTime, 0, 1, FDuration, FAnimationType);
      TInterpolationType.Quartic:
        Result := InterpolateQuart(FTime, 0, 1, FDuration, FAnimationType);
      TInterpolationType.Quintic:
        Result := InterpolateQuint(FTime, 0, 1, FDuration, FAnimationType);
      TInterpolationType.Sinusoidal:
        Result := InterpolateSine(FTime, 0, 1, FDuration, FAnimationType);
      TInterpolationType.Exponential:
        Result := InterpolateExpo(FTime, 0, 1, FDuration, FAnimationType);
      TInterpolationType.Circular:
        Result := InterpolateCirc(FTime, 0, 1, FDuration, FAnimationType);
      TInterpolationType.Elastic:
        Result := InterpolateElastic(FTime, 0, 1, FDuration, 0, 0, FAnimationType);
      TInterpolationType.Back:
        Result := InterpolateBack(FTime, 0, 1, FDuration, 0, FAnimationType);
      TInterpolationType.Bounce:
        Result := InterpolateBounce(FTime, 0, 1, FDuration, FAnimationType);
    end;
  end;
end;

function TGameAnimation.GetTrigger(const Index: TTriggerType): TTrigger;
begin
  Result := FTriggers[Index].Trigger;
end;

procedure TGameAnimation.DoProcess;
begin
  if Assigned(FOnProcess) then
    FOnProcess(Self);
end;

procedure TGameAnimation.DoFinish;
begin
  if Assigned(FOnFinish) then
    FOnFinish(Self);
end;

procedure TGameAnimation.ProcessTick(const ATime, ADeltaTime: Single);
var
  Control: IControl;
begin
  inherited;
  if [csDesigning, csDestroying] * ComponentState <> [] then
    Exit;

  if Supports(Parent, IControl, Control) and (not Control.Visible) then
    Stop;

  if (not FRunning) or FPause then
    Exit;

  if (FDelay > 0) and (FDelayTime <> 0) then
  begin
    if FDelayTime > 0 then
    begin
      FDelayTime := FDelayTime - ADeltaTime;
      if FDelayTime <= 0 then
      begin
        FDelayTime := 0;
        if FInverse then
          FTime := FDuration
        else
          FTime := 0;
        FirstFrame;
        ProcessAnimation;
        DoProcess;
      end;
    end;
    Exit;
  end;

  if FInverse then
    FTime := FTime - ADeltaTime
  else
    FTime := FTime + ADeltaTime;
  if FTime >= FDuration then
  begin
    FTime := FDuration;
    if FLoop then
    begin
      if FAutoReverse then
      begin
        FInverse := True;
        FTime := FDuration;
      end
      else
        FTime := 0;
    end
    else
      if FAutoReverse and (FTickCount = 0) then
      begin
        Inc(FTickCount);
        FInverse := True;
        FTime := FDuration;
      end
      else if FAutoRemove then
        FRunning := False;
  end
  else if FTime <= 0 then
  begin
    FTime := 0;
    if FLoop then
    begin
      if FAutoReverse then
      begin
        FInverse := False;
        FTime := 0;
      end
      else
        FTime := FDuration;
    end
    else
      if FAutoReverse and (FTickCount = 0) then
      begin
        Inc(FTickCount);
        FInverse := False;
        FTime := 0;
      end
      else
        FRunning := False;
  end;

  ProcessAnimation;
  DoProcess;

  if not FRunning and FAutoRemove then
  begin
    if AutoReverse then
      FInverse := FSavedInverse;
    if AniThread <> nil then
      TAniThread(AniThread).RemoveAnimation(Self);
    DoFinish;
  end;
end;

procedure TGameAnimation.Start;
var
  Control: IControl;
  SaveDuration: Single;
begin
  if not FLoop then
    FTickCount := 0;
  if Supports(Parent, IControl, Control) and (not Control.Visible) then
    Exit;
  if AutoReverse then
  begin
    if Running then
      FInverse := FSavedInverse
    else
      FSavedInverse := FInverse;
  end;
//  if (Abs(FDuration) < 0.001) or (Root = nil) or (csDesigning in ComponentState) then
//  begin
//    { immediate animation }
//    SaveDuration := FDuration;
//    try
//      FDelayTime := 0;
//      FDuration := 1;
//      if FInverse then
//        FTime := 0
//      else
//        FTime := FDuration;
//      FRunning := True;
//      ProcessAnimation;
//      DoProcess;
//      FRunning := False;
//      FTime := 0;
//      DoFinish;
//    finally
//      FDuration := SaveDuration;
//    end;
//  end
//  else
  begin
    FDelayTime := FDelay;
    FRunning := True;
    if FInverse then
      FTime := FDuration
    else
      FTime := 0;
    if FDelay = 0 then
    begin
      FirstFrame;
      ProcessAnimation;
      DoProcess;
    end;

    if AniThread = nil then
      FAniThread := TAniThread.Create;

    TAniThread(AniThread).AddAnimation(Self);
    if not AniThread.Enabled then
      Stop
    else
      FEnabled := True;
  end;
end;

procedure TGameAnimation.Stop;
begin
  if not FRunning then
    Exit;

  if AniThread <> nil then
    TAniThread(AniThread).RemoveAnimation(Self);

  if AutoReverse then
    FInverse := FSavedInverse;

  if FInverse then
    FTime := 0
  else
    FTime := FDuration;
//  ProcessAnimation;
//  DoProcess;
  FRunning := False;
  DoFinish;
end;

procedure TGameAnimation.StopAtCurrent;
begin
  if not FRunning then
    Exit;

  if AniThread <> nil then
    TAniThread(AniThread).RemoveAnimation(Self);

  if AutoReverse then
    FInverse := FSavedInverse;

  if FInverse then
    FTime := 0
  else
    FTime := FDuration;
  FRunning := False;
  FEnabled := False;
  DoFinish;
end;

procedure TGameAnimation.StartTrigger(const AInstance: TFmxObject; const APropertyName: string);
var
  LTrigger: TGameAnimationTrigger;
begin
  if AInstance = nil then
    Exit;

  { Try to start inverse animation }
  LTrigger := FTriggers[TTriggerType.Inverse];
  if LTrigger.HasProperty(APropertyName) and LTrigger.CanExecute(AInstance) then
  begin
    Inverse := True;
    Start;
    Exit;
  end;

  { Try to start normal animation }
  LTrigger := FTriggers[TTriggerType.Normal];
  if LTrigger.HasProperty(APropertyName) and LTrigger.CanExecute(AInstance) then
  begin
    if TriggerInverse <> '' then
      Inverse := False;
    Start;
  end;
end;

procedure TGameAnimation.StopTrigger(const AInstance: TFmxObject; const APropertyName: string);
begin
  if AInstance = nil then
    Exit;

  if FTriggers[TTriggerType.Inverse].HasProperty(APropertyName) or FTriggers[TTriggerType.Normal].HasProperty(APropertyName) then
    Stop;
end;

class procedure TGameAnimation.Uninitialize;
begin
  FreeAndNil(FAniThread);
end;

{ TSpriteAnimation }

destructor TSpriteAnimation.Destroy;
var
  TriggerType: TTriggerType;
begin
  for TriggerType := Low(TTriggerType) to High(TTriggerType) do
    FreeAndNil(FTriggers[TriggerType]);
  if AniThread <> nil then
    TAniThread(AniThread).FSprList.Remove(Self);
  inherited;
end;

procedure TSpriteAnimation.Start;
var
  Control: IControl;
  SaveDuration: Single;
begin
  if not FLoop then
    FTickCount := 0;
  if Supports(Parent, IControl, Control) and (not Control.Visible) then
    Exit;
  if AutoReverse then
  begin
    if Running then
      FInverse := FSavedInverse
    else
      FSavedInverse := FInverse;
  end;


  FDelayTime := FDelay;
  FRunning := True;
  if FInverse then
    FTime := FDuration
  else
    FTime := 0;
  if FDelay = 0 then
  begin
    FirstFrame;
//      ProcessAnimation;
//      DoProcess;
  end;

  if AniThread = nil then
    FAniThread := TAniThread.Create;

  TAniThread(AniThread).AddSprite(Self);
  if not AniThread.Enabled then
    Stop
  else
    FEnabled := True;

end;

procedure TSpriteAnimation.Stop;
begin
  inherited;

end;


{ TCustomPropertyAnimation }

procedure TCustomPropertyAnimation.AssignTo(Dest: TPersistent);
var
  DestAnimation: TCustomPropertyAnimation;
begin
  if Dest Is TCustomPropertyAnimation then
  begin
    DestAnimation := TCustomPropertyAnimation(Dest);
    DestAnimation.PropertyName := PropertyName;
  end;
  inherited;
end;

function TCustomPropertyAnimation.FindProperty: Boolean;
var
  Persistent: string;
  Comp: TFmxObject;
  I: Integer;
  T: TRttiType;
  P: TRttiProperty;
  Properties: TList<TRttiProperty>;
begin
  Result := False;

  if (Parent <> nil) and (FPropertyName <> '') then
  begin
    if FInstance = nil then
    begin
      FInstance := Parent;
      FPath := FPropertyName;
      while FPath.Contains('.') do
      begin
        Persistent := GetToken(FPath, '.');
        T := SharedContext.GetType(FInstance.ClassInfo);
        if T <> nil then
        begin
          P := T.GetProperty(Persistent);
          if (P <> nil) and (P.PropertyType.IsInstance) then
            FInstance := P.GetValue(FInstance).AsObject
          else
          if Parent <> nil then
          begin
            for I := 0 to Parent.ChildrenCount - 1 do
              if CompareText(Parent.Children[I].Name, Persistent) = 0 then
              begin
                Comp := Parent.Children[I];
                T := SharedContext.GetType(Comp.ClassInfo);
                if T <> nil then
                begin
                  P := T.GetProperty(FPath);
                  if P <> nil then
                  begin
                    FInstance := Comp;
                    Break;
                  end;
                end;
              end;
          end;
        end;
      end;
      if FInstance <> nil then
      begin

        if not ClonePropertiesCache.TryGetValue(FInstance.ClassName, Properties) then
        begin
          Properties := TList<TRttiProperty>.Create;
          ClonePropertiesCache.Add(FInstance.ClassName, Properties);
        end;

        for P in Properties do
          if P.Name = FPath then
          begin
            FRttiProperty := P;
            Break;
          end;

        if FRttiProperty = nil then
        begin
          T := SharedContext.GetType(FInstance.ClassInfo);
          FRttiProperty := T.GetProperty(FPath);
          if FRttiProperty <> nil then
            Properties.Add(FRttiProperty);
        end;
        Result := FRttiProperty <> nil;
      end;
    end
    else
      Result := True;
  end;
end;

procedure TCustomPropertyAnimation.ParentChanged;
begin
  inherited;
  FInstance := nil;
end;

procedure TCustomPropertyAnimation.SetPropertyName(const AValue: string);
begin
  if not SameText(AValue, PropertyName) then
    FInstance := nil;
  FPropertyName := AValue;
  FTargetName := FTarget.Name + '.' + AValue;
end;

procedure TCustomPropertyAnimation.Start;
begin
  FInstance := FTarget;
  FPath := FPropertyName;
//  if FindProperty then
    inherited;
end;

procedure TCustomPropertyAnimation.Stop;
begin
  inherited;
  FInstance := nil;
end;

{ TFloatAnimation }

procedure TFloatAnimation.AssignTo(Dest: TPersistent);
var
  DestAnimation: TFloatAnimation;
begin
  if Dest is TFloatAnimation then
  begin
    DestAnimation := TFloatAnimation(Dest);
    DestAnimation.StartValue := StartValue;
    DestAnimation.StopValue := StopValue;
    DestAnimation.StartFromCurrent := StartFromCurrent;
  end;
  inherited;
end;

constructor TFloatAnimation.Create(AOwner: TComponent);
begin
  inherited;
  Duration := 0.2;
  FStartFloat := 0;
  FStopFloat := 0;
end;

procedure TFloatAnimation.FirstFrame;
var
  T: TRttiType;
  P: TRttiProperty;
begin
  if StartFromCurrent then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      if (P <> nil) and (P.PropertyType.TypeKind = tkFloat) then
        StartValue := P.GetValue(FInstance).AsExtended;
    end;
  end;
end;

procedure TFloatAnimation.ProcessAnimation;
var
  T: TRttiType;
  P: TRttiProperty;
begin
  if FInstance <> nil then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      if (P <> nil) and (P.PropertyType.TypeKind = tkFloat) then
        P.SetValue(FInstance, InterpolateSingle(FStartFloat, FStopFloat, NormalizedTime));
    end;
  end;
end;

{ TIntAnimation }

procedure TIntAnimation.AssignTo(Dest: TPersistent);
var
  DestAnimation: TIntAnimation;
begin
  if Dest is TIntAnimation then
  begin
    DestAnimation := TIntAnimation(Dest);
    DestAnimation.StartValue := StartValue;
    DestAnimation.StopValue := StopValue;
    DestAnimation.StartFromCurrent := StartFromCurrent;
  end;
  inherited;
end;

constructor TIntAnimation.Create(AOwner: TComponent);
begin
  inherited;
  Duration := 0.2;
  FStartValue := 0;
  FStopValue := 0;
end;

procedure TIntAnimation.FirstFrame;
var
  T: TRttiType;
  P: TRttiProperty;
begin
  if StartFromCurrent then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      if (P <> nil) and (P.PropertyType.TypeKind in [tkInteger, tkFloat]) then
        StartValue := Round(P.GetValue(FInstance).AsExtended);
    end;
  end;
end;

procedure TIntAnimation.ProcessAnimation;
var
  T: TRttiType;
  P: TRttiProperty;
begin
  if FInstance <> nil then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      if (P <> nil) and (P.PropertyType.TypeKind in [tkInteger, tkFloat]) then
        P.SetValue(FInstance, Round(InterpolateSingle(FStartValue, FStopValue, NormalizedTime)));
    end;
  end;
end;

{ TColorAnimation }

procedure TColorAnimation.AssignTo(Dest: TPersistent);
var
  DestAnimation: TColorAnimation;
begin
  if Dest is TColorAnimation then
  begin
    DestAnimation := TColorAnimation(Dest);
    DestAnimation.StartValue := StartValue;
    DestAnimation.StopValue := StopValue;
    DestAnimation.StartFromCurrent := StartFromCurrent;
  end;
  inherited;
end;

constructor TColorAnimation.Create(AOwner: TComponent);
begin
  inherited;
  Duration := 0.2;
  FStartColor := $FFFFFFFF;
  FStopColor := $FFFFFFFF;
end;

procedure TColorAnimation.FirstFrame;
var
  T: TRttiType;
  P: TRttiProperty;
begin
  if StartFromCurrent then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      if (P <> nil) and (P.PropertyType.IsOrdinal) then
        StartValue := TAlphaColor(P.GetValue(FInstance).AsOrdinal);
    end;
  end;
end;

procedure TColorAnimation.ProcessAnimation;
var
  T: TRttiType;
  P: TRttiProperty;
begin
  if FInstance <> nil then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      if (P <> nil) and (P.PropertyType.IsOrdinal) then
        P.SetValue(FInstance, InterpolateColor(FStartColor, FStopColor, NormalizedTime));
    end;
  end;
end;

{ TGradientAnimation }

procedure TGradientAnimation.AssignTo(Dest: TPersistent);
var
  DestAnimation: TGradientAnimation;
begin
  if Dest is TGradientAnimation then
  begin
    DestAnimation := TGradientAnimation(Dest);
    DestAnimation.StartValue := StartValue;
    DestAnimation.StopValue := StopValue;
    DestAnimation.StartFromCurrent := StartFromCurrent;
  end;
  inherited;
end;

constructor TGradientAnimation.Create(AOwner: TComponent);
begin
  inherited;
  Duration := 0.2;
  FStartGradient := TGradient.Create;
  FStopGradient := TGradient.Create;
end;

destructor TGradientAnimation.Destroy;
begin
  FreeAndNil(FStartGradient);
  FreeAndNil(FStopGradient);
  inherited;
end;

procedure TGradientAnimation.FirstFrame;
var
  T: TRttiType;
  P: TRttiProperty;
begin
  if StartFromCurrent then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      if (P <> nil) and (P.PropertyType.IsInstance) then
        StartValue := TGradient(P.GetValue(FInstance).AsObject);
    end;
  end;
end;

procedure TGradientAnimation.ProcessAnimation;
var
  T: TRttiType;
  P: TRttiProperty;
  I: Integer;
  TargetGradientTmp: TGradient;
begin
  if FInstance <> nil then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      if (P <> nil) and P.PropertyType.IsInstance then
      begin
        TargetGradientTmp := TGradient(P.GetValue(FInstance).AsObject);
        for I := 0 to TargetGradientTmp.Points.Count - 1 do
        begin
          if (I < FStopGradient.Points.Count) and (I < FStartGradient.Points.Count) then
            TargetGradientTmp.Points[I].Color :=
              FMX.Utils.InterpolateColor(FStartGradient.Points[I].Color, FStopGradient.Points[I].Color, NormalizedTime);
        end;
        TargetGradientTmp.Change;
      end;
    end;
  end;
end;

procedure TGradientAnimation.SetStartGradient(const Value: TGradient);
begin
  FStartGradient.Assign(Value);
end;

procedure TGradientAnimation.SetStopGradient(const Value: TGradient);
begin
  FStopGradient.Assign(Value);
end;

{ TRectAnimation }

procedure TRectAnimation.AssignTo(Dest: TPersistent);
var
  DestAnimation: TRectAnimation;
begin
  if Dest is TRectAnimation then
  begin
    DestAnimation := TRectAnimation(Dest);
    DestAnimation.StartValue := StartValue;
    DestAnimation.StopValue := StopValue;
    DestAnimation.StartFromCurrent := StartFromCurrent;
  end;
  inherited;
end;

constructor TRectAnimation.Create(AOwner: TComponent);
begin
  inherited;
  Duration := 0.2;
  FStartRect := TBounds.Create(TRectF.Empty);
  FStopRect := TBounds.Create(TRectF.Empty);
  FCurrent := TBounds.Create(TRectF.Empty);
end;

destructor TRectAnimation.Destroy;
begin
  FreeAndNil(FCurrent);
  FreeAndNil(FStartRect);
  FreeAndNil(FStopRect);
  inherited;
end;

procedure TRectAnimation.FirstFrame;
begin
  if StartFromCurrent then
  begin
    if (FRttiProperty <> nil) and FRttiProperty.PropertyType.IsInstance then
      TBounds(FRttiProperty.GetValue(FInstance).AsObject).Assign(FCurrent);
  end;
end;

procedure TRectAnimation.ProcessAnimation;
begin
  if FInstance <> nil then
  begin
    { calc value }
    FCurrent.Left := InterpolateSingle(FStartRect.Left, FStopRect.Left,
      NormalizedTime);
    FCurrent.Top := InterpolateSingle(FStartRect.Top, FStopRect.Top,
      NormalizedTime);
    FCurrent.Right := InterpolateSingle(FStartRect.Right, FStopRect.Right,
      NormalizedTime);
    FCurrent.Bottom := InterpolateSingle(FStartRect.Bottom, FStopRect.Bottom,
      NormalizedTime);

    if (FRttiProperty <> nil) and FRttiProperty.PropertyType.IsInstance then
      TBounds(FRttiProperty.GetValue(FInstance).AsObject).Assign(FCurrent);
  end;
end;

procedure TRectAnimation.SetStartRect(const Value: TBounds);
begin
  FStartRect.Assign(Value);
end;

procedure TRectAnimation.SetStopRect(const Value: TBounds);
begin
  FStopRect.Assign(Value);
end;

{ TBitmapAnimation }

procedure TBitmapAnimation.AssignTo(Dest: TPersistent);
var
  DestAnimation: TBitmapAnimation;
begin
  if Dest is TBitmapAnimation then
  begin
    DestAnimation := TBitmapAnimation(Dest);
    DestAnimation.StartValue := StartValue;
    DestAnimation.StopValue := StopValue;
  end;
  inherited;
end;

constructor TBitmapAnimation.Create(AOwner: TComponent);
begin
  inherited;
  Duration := 0.2;
  FStartBitmap := TBitmap.Create;
  FStopBitmap := TBitmap.Create;
end;

destructor TBitmapAnimation.Destroy;
begin
  FreeAndNil(FStartBitmap);
  FreeAndNil(FStopBitmap);
  inherited;
end;

procedure TBitmapAnimation.ProcessAnimation;
var
  T: TRttiType;
  P: TRttiProperty;
  Value: TPersistent;
  Bitmap: TBitmap;
begin
  if FInstance <> nil then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      if (P <> nil) and (P.PropertyType.IsInstance) then
      begin
        Value := TPersistent(P.GetValue(FInstance).AsObject);
        if (Value <> nil) and (Value is TBitmap) then
        begin
          Bitmap := TBitmap(Value);
          if Inverse then
          begin
            { assign to start }
            Bitmap.SetSize(FStopBitmap.Width, FStopBitmap.Height);
            { draw final with alpha }
            if Bitmap.Canvas.BeginScene then
            try
              Bitmap.Canvas.Clear(0);
              Bitmap.Canvas.DrawBitmap(FStopBitmap,
                RectF(0, 0, Bitmap.Width, Bitmap.Height),
                RectF(0, 0, FStopBitmap.Width / Bitmap.BitmapScale, FStopBitmap.Height / Bitmap.BitmapScale),
                NormalizedTime);
              Bitmap.Canvas.DrawBitmap(FStartBitmap,
                RectF(0, 0, Bitmap.Width, Bitmap.Height),
                RectF(0, 0, FStartBitmap.Width / Bitmap.BitmapScale, FStartBitmap.Height / Bitmap.BitmapScale),
                1 - NormalizedTime);
            finally
              Bitmap.Canvas.EndScene;
            end;
          end
          else
          begin
            { assign to start }
            Bitmap.SetSize(FStartBitmap.Width, FStartBitmap.Height);
            { draw final with alpha }
            if Bitmap.Canvas.BeginScene then
            try
              Bitmap.Canvas.Clear(0);
              Bitmap.Canvas.DrawBitmap(FStartBitmap,
                RectF(0, 0, Bitmap.Width, Bitmap.Height),
                RectF(0, 0, FStartBitmap.Width / Bitmap.BitmapScale, FStartBitmap.Height / Bitmap.BitmapScale),
                1 - NormalizedTime);
              Bitmap.Canvas.DrawBitmap(FStopBitmap,
                RectF(0, 0, Bitmap.Width, Bitmap.Height),
                RectF(0, 0, FStopBitmap.Width / Bitmap.BitmapScale, FStopBitmap.Height / Bitmap.BitmapScale),
                NormalizedTime);
            finally
              Bitmap.Canvas.EndScene;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TBitmapAnimation.SetStartBitmap(Value: TBitmap);
begin
  FStartBitmap.Assign(Value);
end;

procedure TBitmapAnimation.SetStopBitmap(Value: TBitmap);
begin
  FStopBitmap.Assign(Value);
end;

{ TBitmapListAnimation.TAnimationBitmap }

procedure TBitmapListAnimation.TAnimationBitmap.ReadStyleLookup(Reader: TReader);
begin
  if FAnimation <> nil then
    FAnimation.AnimationLookup := Reader.ReadString;
end;

{ TBitmapListAnimation }

procedure TBitmapListAnimation.AssignTo(Dest: TPersistent);
var
  DestAnimation: TBitmapListAnimation;
begin
  if Dest is TBitmapListAnimation then
  begin
    DestAnimation := TBitmapListAnimation(Dest);
    DestAnimation.AnimationBitmap := AnimationBitmap;
    DestAnimation.AnimationLookup := AnimationLookup;
    DestAnimation.AnimationCount := AnimationCount;
    DestAnimation.AnimationRowCount := AnimationRowCount;
  end;
  inherited;
end;

constructor TBitmapListAnimation.Create(AOwner: TComponent);
begin
  inherited;
  Duration := 0.2;
  FAnimationBitmap := TAnimationBitmap.Create;
  TAnimationBitmap(FAnimationBitmap).FAnimation := Self;
  FAnimationCount := 1;
  FAnimationRowCount := 1;
  FLastAnimationStep := 0;
end;

destructor TBitmapListAnimation.Destroy;
begin
  FreeAndNil(FAnimationBitmap);
  inherited;
end;

procedure TBitmapListAnimation.RefreshBitmap(const WorkBitmap: TBitmap);
var
  ResourceObject: TFmxObject;
  BitmapObject: IBitmapObject;
  MultiResObject: IMultiResBitmapObject;
  Item: TCustomBitmapItem;
begin
  if not AnimationLookup.IsEmpty then
  begin
    ResourceObject := FMX.Types.FindStyleResource(AnimationLookup);
    if Supports(ResourceObject, IMultiResBitmapObject, MultiResObject) and (WorkBitmap <> nil) then
    begin
      Item := MultiResObject.MultiResBitmap.ItemByScale(WorkBitmap.BitmapScale, False, False);
      if Item <> nil then
      begin
        AnimationBitmap.Assign(Item.Bitmap);
        Exit;
      end;
    end;
    if Supports(ResourceObject, IBitmapObject, BitmapObject) then
      AnimationBitmap.Assign(BitmapObject.Bitmap);
  end;
end;

procedure TBitmapListAnimation.ProcessAnimation;
var
  T: TRttiType;
  P: TRttiProperty;
  Value: TObject;
  TopPos, LeftPos, CurrentIndex: Integer;
  NowValue: Single;
  SourceBitmap: TBitmap;
  WorkBitmap: TBitmap;
  AnimationColCount: Integer;
begin
  if FInstance <> nil then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      if (P <> nil) and (P.PropertyType.IsInstance) then
      begin
        Value := P.GetValue(FInstance).AsObject;
        if Value is TBitmap then
          WorkBitmap := TBitmap(Value)
        else
          WorkBitmap := nil;

        if WorkBitmap <> nil then
        begin
          if (FAnimationBitmap.BitmapScale <> WorkBitmap.BitmapScale) and (FAnimationLookup <> '') then
            RefreshBitmap(WorkBitmap);

          FAnimationBitmap.BitmapScale := WorkBitmap.BitmapScale;
        end;

        SourceBitmap := FAnimationBitmap;

        if (WorkBitmap <> nil) and not (SourceBitmap.IsEmpty) then
        begin
          NowValue := InterpolateSingle(0, FAnimationCount, NormalizedTime);

          if FAnimationCount mod FAnimationRowCount = 0 then
            AnimationColCount := FAnimationCount div FAnimationRowCount
          else
            AnimationColCount := FAnimationCount div FAnimationRowCount + 1;

          WorkBitmap.SetSize(SourceBitmap.Width div AnimationColCount, SourceBitmap.Height div FAnimationRowCount);

          CurrentIndex := Trunc(NowValue);
          if CurrentIndex > FAnimationCount - 1 then
            CurrentIndex := FAnimationCount - 1;

          LeftPos := (CurrentIndex mod AnimationColCount) * (SourceBitmap.Width div AnimationColCount);
          TopPos := (CurrentIndex div AnimationColCount) * (SourceBitmap.Height div AnimationRowCount);

          if WorkBitmap.Canvas.BeginScene then
          try
            WorkBitmap.Canvas.Clear(0);
            WorkBitmap.Canvas.DrawBitmap(SourceBitmap, TRectF.Create(LeftPos, TopPos, LeftPos + WorkBitmap.Width,
              TopPos + WorkBitmap.Height), TRectF.Create(0, 0, WorkBitmap.Width / WorkBitmap.BitmapScale,
              WorkBitmap.Height / WorkBitmap.BitmapScale), 1);
          finally
            WorkBitmap.Canvas.EndScene;
          end;
        end;
      end;
    end;
  end;
end;

procedure TBitmapListAnimation.SetAnimationBitmap(Value: TBitmap);
begin
  FAnimationBitmap.Assign(Value);
end;

procedure TBitmapListAnimation.SetAnimationLookup(const Value: string);
begin
  if FAnimationLookup <> Value then
  begin
    FAnimationLookup := Value;
    RefreshBitmap;
  end;
end;

procedure TBitmapListAnimation.SetAnimationRowCount(const Value: Integer);
begin
  FAnimationRowCount := Value;
  if FAnimationRowCount < 1 then
    FAnimationRowCount := 1;
end;

{ Key Animation }

{ TKey }

procedure TKey.Assign(Source: TPersistent);
begin
  if Source is TKey then
    FKey := TKey(Source).Key
  else
    inherited;
end;

procedure TKey.SetKey(const Value: Single);
begin
  FKey := Value;
  if FKey < 0 then
    FKey := 0;
  if FKey > 1 then
    FKey := 1;
end;

{ TKeys }

function TKeys.FindKeys(const Time: Single; var Key1, Key2: TKey): Boolean;
var
  I: Integer;
begin
  Result := False;
  if Count < 2 then
    Exit;
  for I := 0 to Count - 2 do
    if (Time >= TKey(Items[I]).Key) and (Time <= TKey(Items[I + 1]).Key) then
    begin
      Result := True;
      Key1 := TKey(Items[I]);
      Key2 := TKey(Items[I + 1]);
      Exit;
    end;
end;

{ TColorKeyAnimation }

procedure TColorKeyAnimation.AssignTo(Dest: TPersistent);
var
  DestAnimation: TColorKeyAnimation;
begin
  if Dest is TColorKeyAnimation then
  begin
    DestAnimation := TColorKeyAnimation(Dest);
    DestAnimation.Keys := Keys;
    DestAnimation.StartFromCurrent := StartFromCurrent;
  end;
  inherited;
end;

constructor TColorKeyAnimation.Create(AOwner: TComponent);
begin
  inherited;
  FKeys := TKeys.Create(TColorKey);
end;

destructor TColorKeyAnimation.Destroy;
begin
  FreeAndNil(FKeys);
  inherited;
end;

procedure TColorKeyAnimation.FirstFrame;
var
  T: TRttiType;
  P: TRttiProperty;
begin
  if StartFromCurrent then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      if (P <> nil) and P.PropertyType.IsOrdinal and (Keys.Count > 0) then
        TColorKey(Keys.Items[0]).Value := P.GetValue(FInstance).AsOrdinal;
    end;
  end;
end;

procedure TColorKeyAnimation.ProcessAnimation;
var
  T: TRttiType;
  P: TRttiProperty;
  Key1, Key2: TKey;
begin
  if FInstance <> nil then
  begin
    if FKeys.FindKeys(NormalizedTime, Key1, Key2) then
    begin
      if (TFloatKey(Key2).Key - TFloatKey(Key1).Key) = 0 then
        Exit;
      T := SharedContext.GetType(FInstance.ClassInfo);
      if T <> nil then
      begin
        P := T.GetProperty(FPath);
        if (P <> nil) and P.PropertyType.IsOrdinal then
          P.SetValue(FInstance,
            InterpolateColor(TColorKey(Key1).Value, TColorKey(Key2).Value,
              (NormalizedTime - TFloatKey(Key1).Key) / (TFloatKey(Key2).Key - TFloatKey(Key1).Key)));
      end;
    end;
  end;
end;

procedure TColorKeyAnimation.SetKeys(const Value: TKeys);
begin
  FKeys.Assign(Value);
end;

{ TFloatKeyAnimation }

procedure TFloatKeyAnimation.AssignTo(Dest: TPersistent);
var
  DestAnimation: TFloatKeyAnimation;
begin
  if Dest is TFloatKeyAnimation then
  begin
    DestAnimation := TFloatKeyAnimation(Dest);
    DestAnimation.Keys := Keys;
    DestAnimation.StartFromCurrent := StartFromCurrent;
  end;
  inherited;
end;

constructor TFloatKeyAnimation.Create(AOwner: TComponent);
begin
  inherited;
  FKeys := TKeys.Create(TFloatKey);
end;

destructor TFloatKeyAnimation.Destroy;
begin
  FreeAndNil(FKeys);
  inherited;
end;

procedure TFloatKeyAnimation.FirstFrame;
var
  T: TRttiType;
  P: TRttiProperty;
begin
  if StartFromCurrent then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      if (P <> nil) and P.PropertyType.IsOrdinal and (Keys.Count > 0) then
        TFloatKey(Keys.Items[0]).Value := P.GetValue(FInstance).AsExtended;
    end;
  end;
end;

procedure TFloatKeyAnimation.ProcessAnimation;
var
  T: TRttiType;
  P: TRttiProperty;
  Key1, Key2: TKey;
begin
  if FInstance <> nil then
  begin
    if FKeys.FindKeys(NormalizedTime, Key1, Key2) then
    begin
      if (TFloatKey(Key2).Key - TFloatKey(Key1).Key) = 0 then
        Exit;
      T := SharedContext.GetType(FInstance.ClassInfo);
      if T <> nil then
      begin
        P := T.GetProperty(FPath);
        if (P <> nil) and (P.PropertyType.TypeKind = tkFloat) then
          P.SetValue(FInstance, InterpolateSingle(TFloatKey(Key1).Value, TFloatKey(Key2).Value,
            (NormalizedTime - TFloatKey(Key1).Key) / (TFloatKey(Key2).Key - TFloatKey(Key1).Key)))
        else if (P <> nil) and P.PropertyType.IsOrdinal then
          P.SetValue(FInstance, Round(InterpolateSingle(TFloatKey(Key1).Value, TFloatKey(Key2).Value,
            (NormalizedTime - TFloatKey(Key1).Key) / (TFloatKey(Key2).Key - TFloatKey(Key1).Key))));
      end;
    end;
  end;
end;

procedure TFloatKeyAnimation.SetKeys(const Value: TKeys);
begin
  FKeys.Assign(Value);
end;

{ TColorKey }

procedure TColorKey.Assign(Source: TPersistent);
begin
  if Source is TColorKey then
    FValue := TColorKey(Source).Value
  else
    inherited;
end;

{$IFDEF ANDROID}
{ TTimerThread }

constructor TTimerThread.Create;
begin
  inherited;
  FEnabledEvent := TEvent.Create;
  Interval := 1;
  FEnabled := False;
end;

destructor TTimerThread.Destroy;
begin
  FreeAndNil(FEnabledEvent);
  inherited;
end;

procedure TTimerThread.BeforeDestruction;
begin
  FEnabledEvent.setEvent;
  inherited;
end;

procedure TTimerThread.DoInterval;
begin
  if Assigned(FTimerEvent) then
    FTimerEvent(Self);
end;

procedure TTimerThread.Execute;
begin
  while not Terminated do
  begin
    Sleep(Interval);
    TThread.Synchronize(nil,
      procedure
      begin
        DoInterval;
      end);
    FEnabledEvent.WaitFor;
  end;
end;

procedure TTimerThread.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if FEnabled then
      FEnabledEvent.SetEvent
    else
      FEnabledEvent.ResetEvent;
  end;
end;

procedure TTimerThread.SetInterval(const Value: Cardinal);
begin
  FInterval := Max(Value, 1);
end;

{ TThreadTimer }

constructor TThreadTimer.Create(AOwner: TComponent);
begin
  inherited;
  FThread := TTimerThread.Create;
end;

procedure TThreadTimer.BeforeDestruction;
begin
  FThread.Free;
  inherited;
end;

procedure TThreadTimer.SetEnabled(Value: Boolean);
begin
  inherited;
  FThread.Enabled := Value;
end;

procedure TThreadTimer.SetInterval(Value: Cardinal);
begin
  inherited;
  FThread.Interval := Value;
end;

procedure TThreadTimer.SetOnTimer(Value: TNotifyEvent);
begin
  inherited;
  FThread.OnTimer := Value;
end;

procedure TThreadTimer.UpdateTimer;
begin
  // Don't invoke inherited method, because we take care under alternative timer implementation with Thread.
end;
{$ENDIF ANDROID}

initialization
//  RegisterFmxClasses([TColorAnimation, TGradientAnimation, TFloatAnimation, TIntAnimation,
//    TRectAnimation, TBitmapAnimation, TBitmapListAnimation, TColorKeyAnimation,
//    TFloatKeyAnimation]);
  TGameAnimation.AniFrameRate := TGameAnimation.DefaultAniFrameRate;
finalization
  TGameAnimator.Uninitialize;
  TGameAnimation.Uninitialize;
end.


