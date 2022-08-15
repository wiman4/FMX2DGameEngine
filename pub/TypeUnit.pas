unit TypeUnit;

interface

uses
  MeiStream, System.Generics.Collections;

const
  WM_SYNC_OBJ = $400 + 1044;
    somDeletePlayer = 1;
    somGameSockDisconnect = 2;
    somDeleteSock = 3;
    somRemoveObj = 4;
    somFreeObj = 5;
    somSendCSockBuff = 6;
    somOpenCSock = 7;
    somCloseCSock = 8;
    somClientConnected = 11;
    somClientDisconnected = 12;
    somDeleteBattleField = 13;
  WM_DB_Event = $400 + 1045;
  WM_SYNC_FUNC_NUM = $400 + 1046;
  WM_SYNC_FUNC_OBJ = $400 + 1047;

  GAME_OBJ_FLAG = $20140613;

  MaxGold = 2000000000;
  MaxCharCount = 4;
  MaxGameObj = 8192;
  MaxFriendCount = 65;
  MaxEquipCount = 6;
  MaxZhuangshiCount = 3;
  MinBackpackSize = 40;
  MaxBackpackSize = 120;
  MaxTradepackSize = 6;
  MaxStorageSize = 400;
  MaxStoragePageSize = 40;
  MaxSkillCount = 64;
  MaxSubSkillCount = 14;
  MaxRevivePoint = 10;
  MaxMapList = 1024;
  MaxPartyMember = 5;
  MaxZhenfaID = 8;
  MaxFxq = 600;
  MaxGamePlayer = 50;
  MaxPetCount = 12;
  MaxPetEquipCount = 3;
  MaxPetSkillCount = 16;
  PetEqSkillId = 10000;
  PetBindSkillId = 20000;
  PetInfinitedLife = $FA00;
  MaxMountCount = 2;
  MaxMountSkillCount = 8;
  MaxQuestCount = 10;
  YunbiaoQuestId = 93001;
  YunbiaoItemId = 20354;
  YunbiaoMonsterIds: array[0..3] of Integer = (1050, 1150, 1045, 1040);
  MaxArenaList = 3;
  MaxPlayerDailyStatusCount = 32;
    pdsPKPunish = 0;
    pdsQifu = 1;
    pdsShimen = 2;
      MaxShimenTimes = 20;
    pdsZhuogui = 3;
    pdsYunbiao = 4;
      MaxYunBiaoTimes = 20;
      YunbiaoDepositMoney = 10000;
  MaxPlayerMonthlyStatusCount = 32;
  MaxPlayerStatusCount = 64;
    psChangeName = 0;
    psPK = 1;
      pkNormal = 1;
      pkForce  = 2;
    psLastForcePKTime = 2;
    psLastRenqiDonateTime = 3;
    psOfflineTime = 4;
    psYunbiao = 5;

  MaxEqTexiaoCount = 4;
  MaxEqTejiCount = 4;

  MaxSideCount = 11;

  //时间常数
  GCD_Time = 1;
  RCV_Time = 5000;
  PKBuffTime = 24 * 3600;

  //距离类常数
  MaxSeenDis = 32.0;
  BaseSpeed = 8.0/1000;
  MonBaseSpeed = 3.6/1000;

  //属性常数
  MaxRenqi = 800;
  MaxYinde = 1000;

  MinPlayerGUID = 1000001;
  MinPetGUID = $40000001;
  MinGuildGUID = 1;
  MinMonsterGUID = $20000000;
  MinItemGuid = $03000000;
  MinNpcGuid = $10000000;

  otPlayer = 1;
  otPet = 2;
  otMonster = 6;
  otSummon = 8;
  otPetMonster = 6;
  otNpc = 256;
  otTeleport = 512;
  otInstNpc = 1024;
  otQuestNpc = 2048;
  otGM = -1;
  otPetSummon = 10;

  ioBackpack = 0;
  ioPetEquip = 2;
  ioPlayerTrade = 3;
  ioNpcShop = 4;
  ioStorage = 5;
  ioMall = 8;
  ioMarket = 10;

  dtDamage = 1;
  dtHeal   = 2;
  dtAoe    = 3;
  dtBuff   = 4;
  dtDeBuff = 5;

  skAttack = -1;
  skDefence = -2;
  skSummonPet = -3;
  skProtect = -4;
  skFlee = -5;
  skFleeFail = -6;
  skFriendlyAtk = -7;

  efHit = 1;
  efDefence = 2;
  efProtect = 3;
  efDodge = 4;
  efParry = 5;
  efCritical = 6;
  skCapture = -7;

  //封包常数
  ccShakeHand = $1C;
  ccGameSys   = $D1;
  cc06        = 06;
  ccServerId  = 07;
  ccEncKey    = 07;
  ccLogin =     01;
  ccChooseChar= 02;
  ccCreateChar= 03;
  ccLogin2    = $0B;
  ccSync      = $0F;
  ccAddProp   = $10;
  ccLevUp     = $11;
  ccLogout    = $15;
  ccPlayerInfo= $16;
  ccMall      = $17;
    csMallOpen = 1;
    csMallItemInfo = 2;
    csMallBuy = 3;
    csMallCharge = 4;
  ccReconnect = $18;
  ccClickBox  = $20;
  ccPlayerFunc= $58;
    pfFxqRec = 1;
    pfFxqCity = 2;
    pfFxqPos = 3;
    pfJubao = 7;
    pfEnterGame = $1C;
    pfConfirm = $2D;
    pfBuff = $3E;
//    pfChangeName = $56;
    pfFxqCharge = $6B;
    pfZhuangshiSwitch = $73;
    pfBackpackExpand = $78;
    pfFxqDel = $99;
    pfStoneEquip = $9A;
  ccQuest = $31;
    csQDelete = 1;
  ccFriend    = $32;
  ccMove = $36;
  ccFacing = $37;
  ccTitle = $43;
  ccShortCut = $44;
  ccEqGuaxiang = $4D;
    csEqGxExpBindgold = 1;
    csEqGxQuit = 2;
    csEqGxTBackpack = 3;
    csEqGxExpGold = 4;
    csEqGxTEquip = 5;
    csEqGxBindgold = 7;
  ccSysMsg = $92;
  ccChat = $95;
  ccNotify = $96;
  cc9D = $9D;
  cc34 = $34;
type
  TNotifyType = (ntNone, ntPartyInvite, ntPartyReturn, ntGuildInvite);

  TActionType = (acNone, acDps, acMDps, acHeal, acFeng, acTank);

const
  ccItemMove = $21;
  ccItemUse = $22;
  ccItemGetInfoBag = $23;
  ccItemAutoUse = $24;
//  ccItem
  ccSysOption = $2C;
  ccTradeAbout = $40;
  ccTradeItemInfo = $91;
  ccItemStorageAbout = $47;
    csStorageItemDeposit = 1;
    csStorageItemWithdraw = 2;
    csStorageItemInfo = 3;
    csStorageItemMove = 4;
    csStorageShowPage = 9;
    csStorageBuySlot = $14;

  ccItemBuy = $63;
  ccQuestTrigger = $64;
  ccItemGetInfoShop = $65;
  ccComposeAbout = $66;
    csCARyby = $24;
  ccSkillStudy = $67;
  ccSmithAbout2 = $68;
    csSARepair = 1;
    csSAStoneSwitch = $18;
    csSATejiChange = $19;
    csSATexiaoChange = $1A;
    csSARybyCompose = $24;
    csSAGuaxiangSwitch = $25;
  ccItemGive = $69;
  ccAttack = $6B;
  ccMarketAbout = $70;
  ccTargetChoose = $71;
    csTCConsumablesTarget = 0;
    csTCGuaxiang = 1;
    csTCPetSkillBind = 2;
    csTCPetSkillUnbind = 3;
    ctPetJinxiu = 4;

  ccSkillUse = $7D;

  ccBattleCharAtk = $A0;
  ccBattleCharSkill = $A1;
  ccBattleCharTeji = $A2;
  ccBattleCharDef = $A3;
  ccBattleCharFlee = $A4;
  ccBattleCharUseItem = $A5;
  ccBattleCharSummonPet = $A6;
  ccBattleCharProtect = $A7;

  ccBattlePetAtk = $A8;
  ccBattlePetSkill = $A9;
  ccBattlePetDef = $AA;
  ccBattlePetUseItem = $AB;
  ccBattlePetProtect = $AC;
  ccBattlePetCapture = $AD;
  ccBattlePetFlee = $AE;

  ccPartyAbout = $48;

  ccGuildAbout = $51;

  ccMountAbout = $87;
  ccPetAbout = $8A;
    pfDel = 1;
    pfZzChk = 2;
    pfChgName = 3;
    pfActivate = 4;
    pfAddAttr  = 7;
  ccPetEnhence = $45;
    peLianhua = 1;
    peSkillStudy = 2;
    peRansePetAndType = 3;
    peHtdAmount = 4;
    peXibaobao = 5;
    peRanse = 7;
    peRanSkill = 9;

  ccTalkOpen = $60;
  ccTalkOption = $62;
  ccTalkInputText = $73;

  ccPreview = $CC;


  ptLogin =  1;
    scServerId = $10;
    scShakeHand = $11;
    scCharCnt   = $12;
    scMapLogin = $FF;
    scCharData = $13;
    scLogin2 = $1C;
    scPlayerData = $20;
    scCharListOver = $2B;
    scMapChange = $70;
    scGameFuncs = $68;
      gfMain = 1;
        mfLottery = 2;
        mfQifu = 3;
        mfXiulianBaoxiang = 4;
        mfGuaxiang = $11;
      gfQuit = 2;
        qfGuaxiang = $11;
      gfUI = 3;
        ufGuaxiangSucc = $11;
      gfCosts = 4;
        cfGuaxiang = $11;

    scSysInfo = $69;
      siGameHelp = 5;
      siChargeURL = $1B;
      siSysTime = $36;
      siConfirm = $2D;
        ctPetDelete = 1;
        ctPartyCreate = 2;
        ctPetRanse = 3;
        ctPetUseShiwu = 4;
        ctQuitGuild = 5;
        ctStoneSwitch = 6;
        ctTejiChange = 7;
        ctTexiaoChange = 8;
        ctGuaxiangSwitch = 9;
        ctQuestDelete = 10;
        ctChangeName = 11;
        ctRybyCompose = 12;
//        ctPetXbb_Shiwu = 5;
      siInsufficientJinbi = $2E;
      siPetRanse = $3B;
      siSysTimeUnix = $4C;
      siConfirm2 = $7C;
      siFxqGUI = $9C;
      siStoneGUI = $9D;
      siFxqUsed = $A5;
      siUILimite = $AB;

    scSync = $0F;
    scLogout = $25;
    scLoginErr = $18;
    scSysOption = $2C;
      soLianyao = 1;
      soBabyStudy = 2;
      soEquipTeji = 3;
      soJiagong = 6;
      soSongli = 7;
      soGaiming10to20 = 8;
      soRanse = 09;
      soPetShiwu = $0B;
      soKahao = $0C;
      soFeixingfu = $0D;
      soPetRanse = $0E;
      soStoneCompose = $2F;


    scSysOption2 = $D1;
      soPhoneIdCode = $12;
      soSilentBroken = $18;
      soCharApplyDelete = $19;
      soItemLink = $2E;
      soChangeName = $37;
        cnrNormal = 1;
        cnrSameName = 2;
        cnrSucc = 3;
        ct10to20Free = 1;
        ctFree = 2;
        ctJinbi = 3;
        ctSucc = 1;
        ctFail = 5;

  ptChat =   2;
    scItemProduct = $B0;
    scNotify = $B3;
    scMsgWithPortrait = $B4;
      mpItem = 0;
      mpSymbol = 1;
      mpModel = 3;
      mpPortrait = 4;
      mpSkillIcon = 7;
    scMessage = $B5;
    scSysMsg = $B6;
      smMsg = 1;
      smNewMsg = 2;
      smFriendApply = 8;
    scChat = $B7;
    scBattleMsg = $B8;
    scLog = $B9;

  ptAction = 3;
    scPlayerExist = $72;
    scNpcExist = $73;
    scObjEffect = $76;
      efLevUp = $0C;
    scObjInbattle = $78;
    scPlayerOutLook = $7a;
    scPlayerInfo = $52;
    scObjDisappear = $55;
    scObjMove = $56;
    scSysNotify = $57;
      snFriendApplyConfirm = 1;
      snFriendApply = 2;
      snFriendSet = 5;
      snFriendInfo = 6;
      snPlayerInfo = 7;
      snTitleColor = $11;
      snFriendStat = $20;
      snFriendSysMsg = $23;
    scQuest = $58;
    scObjBuff = $5A;
      btLast = 2;
      btSecs = 3;
      bfMount = $E9;
    scObjBuffOver = $5B;

    scObjDeBuff = 5;
    scObjHP = 6;
    scObjEquips = 7;
    scObjTransfer = 8;
    scObjRevive = 101;
  ptItem =   4;
    scItemUse = 1;
    scItemGet = $31;
    scItemDelete = $32;
    scItemAmtChg = $33;
    scItemMove = $34;
    scItemInfo = $DB; //$B2; ver.224
    scItemInfo2 = $B2;
    scItemGold = 5;
    scItemSell = 11;
    scItemBuy = 12;
    scItemTrade = 21;
    scItemTradeGold = 22;
    scItemBackPackFull = 101;

    scMall = $28;
      ssMallList = 1;

    scMarketAbout = $AD;

  ptSkill =  5;
    scSkillAbout = $51;
      pcSkillJichuFuzhu = 1;
      pcSkillShimen = 2;
      pcSkillShortCut = 3;
      pcSkillTeji = 4;
      pcTitleSkill = 5;
        tsGuild = $6E;
        tsGovernor = $6F;

    scBattleStart = $80;
    scBattleOver = $81;
    scBattleRound = $82;
    scBattleMember = $83;
    scBattleOut = $84;
    scBattleAttack = $85;
    scBattleProtect = $89;
    scBattleSkill = $8A;
    scBattleDamage = $8B;
    scBattleBuff = $8C;
    scBattleHP = $8D;
    scBattleReady = $8E;
    scBattleNpcChat = $8F;
    scBattleDie1 = $90;
    scBattlePetOwner = $91;
    scBattleCapture = $92;
    scBattleRoundTime = $97;
    scBattleSkillPwr = $98;
    scBattlePetStatus = $9A;
    scBattleActionWords = $9C;
    scBattleMpEffect = $D3;
    scSkillUse = 3;
  ptSelfProp = 6;
    scSelfHPMP = 1;
    scLevExp = 2;
    scSelfAttribute = 3;
    scSelfAddAttribute = 4;
    scPropChange = $21;
      pcOutLook = 4;
      pcName = 6;
      pcLev  = 7;
      pcGold = 8;
      pcBindGold = $0B;
      pcSWB = $0C;
      pcExp = $11;
      pcRExp = $12;
      pcMenPai = $15;
      pcGuildName = $16;
      pcTitleName = $17;
      pcHP = $18;
      pcWP = $19;
      pcMaxHP = $1A;
      pcMP = $1B;
      pcMaxMP = $1C;
      pcSP = $1D;

      pcHuoli = $1F;
      pcMaxHuoli = $20;
      pcTili = $21;
      pcMaxTili = $22;
      pcAtk = $23;
      pcDef = $24;
      pcSpd = $25;
      pcSpr = $26;
      pcVit = $27;
      pcInt = $28;
      pcStr = $29;
      pcDur = $2A;
      pcAgi = $2B;
      pcPt  = $2C;
      pcJinbi = $2F;
      pcPetMaxCount = $33;
      pcBackpackSize = $34;
      pcHasGuild = $3E;
      pcGuildId = $4E;
      pcFxq = $4D;
//      pcBuff = $7A;
//      pcDebuff = $7B;
      pcMax = $7F;

    scSocialPropChange = $26;
      spMenpaiGongxian = 1;
      spShanE = 2;
      spXiayi = 4;
      spRenqi = 5;
      spSanjieWeiwang = 6;
      spChubieWeiwang = 8;
      spHaoxinzhi = 9;
      spDiaoyuJifen = 10;
      spYinde = 21;

    scPreview = $CC;

  //pet
    scPetAbout = $4A;
      pcPetList = 1;
      pcPetSkillList = 2;
      pcPetEquips = 3;
      pcPetActivate = 4;
      pcPetZz = 5;
      pcPetProp = 6;
      pcPetBind = 7;

      pcPetHP = 1;
      pcPetMP = 2;
      pcPetLoyalty = 3;
      pcPetExp = 4;
      pcPetLife = 5;
      pcPetMaxHp = 6;
      pcPetMaxMp = 7;

      pcPetAtk = $23;
      pcPetDef = $24;
      pcPetSpd = $25;
      pcPetSpr = $26;
      pcPetVit = $27;
      pcPetInt = $28;
      pcPetStr = $29;
      pcPetDur = $2A;
      pcPetAgi = $2B;
      pcPetPt = $29;
    scPetEnhenceAbout = $5F;
      ehPetSkill = 1;
      ehPetLianhua = 2;
      ehPetLianhuaResult = 3;
      ehPetHTDCost = 4;
      ehPetXibaobao = 5;
      ehPetLianyao = 6;

      ehPetRanse = 8;
      ehPetRandomSkill = 9;
      ehPetRanSkillList = $0A;

  //mount
    scMountAbout = $47;
      pcMountList = 4;
      pcMountOption = 3;
      pcMountData = 5;
      pcMountHome = 6;
      pcMountActivate = 7;

  //storage
    scStorage = $61;

  //Party
    scPartyAbout = $62;
      pcPartyStatus = 1;
      pcPartyMemberList = 2;
      pcPartyInivite = 3;
      pcPartyApplyList = 5;

      pcPartyMemberAFK = 7;
      pcPartyReturnRequest = 8;
    scPartyStatusDispatch = $77;

    scGuildAbout = $64;
      gsGuildList = 1;
      gsGuildCreate = 2;
      gsGuildInfo = 4;
      gsGuildData = 5;
      gsGuildCreateSucc = 7;
      gsGuildNotice = $0A;
      gsGuildMemberList = $14;
      gsGuildMemberInfo = $15;
      gsGuildMemberAuthority = $16;
      gsGuildMemberPwr = $17;
      gsGuildMemberLeave = $19;
      gsGuildMemberIn = $21;
      gsGuildJianshe = $28;
      gsBuyGuildItem = $4A;
      gsXuetuZhuanZheng = $50;
      gsChangeZongzhi = $52;
      gsCreateHongbao = $5B;
      gsGetHongbao = $5D;
      gsHongbaoOver = $5E;
      gsApplyList = $64;

    scFriendAbout = $65;
    scFriendDelete = $66;

  scFunctionTarget = $A1;
    ftItemFunction = 0;
    ftPetSkillBind = 1;
    ftEquipFunction = 5;
    ftPetJinxiu = 7;

  scNpcTalkText = $A8;
  scNpcInputQuery = $A9;
  scNpcTalkDefaultText = $A0;
  scNpcBuyList = $A3;
  scNpcSkillList = $A5;
  scNpcGive = $A6;
  scNpcSmith = $AA;
  scNpcSmith2 = $A2;
    ssNSRepair = 1;
    ssNSCompose = 2;
    ssNSEffectSwitch = 3;
    ssNSTaozhuang = 4;
  scNpcCost = $AB;
    ssNCRepair = 1;
    ssNCEffectSwitch = 2;

  //Instance
  scInstNpcAbout = $BC;


  scTradeAbout = $6E;
//    scTradeInvite = 1;
//    scTradeAccept = 2;
    tsTradeStart  = 1;
    tsTradeCancel = 2;
    tsTradeLock   = 3;
    tsTradeConfirm= 4;
    tsTradeSucc   = 5;
    tsTradeItemInfo = 11;
    scTradeTargetSlotFull = 6;
    scTradeRefuse = 7;
    scTradeTooSoon= 8;
    scTradeOccupied=9;

  atAtk  = 0;
  atDef  = 1;
  atSpd  = 2;
  atSpr  = 3;
  atVit  = 4;
  atInt  = 5;
  atStr  = 6;
  atDur  = 7;
  atAgi  = 8;
  atPoint= 9;
  atMp = 9;

//  atDex  = 5;
//  atMAtk = 8;
//  atMDef = 10;
  atPerAtk = 11;
  atPerMAtk = 12;
  atPerDef = 13;
  atPerMdef = 14;
  atCri = 15;
  atDodge = 16;
  atMDodge = 17;
  atHit = 18;
  MaxAttribute = 18;

  esFrozen = 1;
  esStun   = 2;
  esFear   = 4;
  esPoison = 8;
  esFrozenStunFear = 7;

  pcHPMP = 1;
  pcBuff = 2;
  pcDeBuff = 4;
  pcAttrPoint = 8;

  mtTradeInvite = 31;

  ttNone = 0;
  ttNpc = 1;
  ttBank = 2;
  ttPlayer = 3;

const
  MaxBuffCount = 8;

  MoveDirs: array[0..8] of array[0..1] of Integer = ((0,0),(1,-1),(-1,-1),(-1,1),(1,1),(0,-1),(-1,0),(0,1),(1,0));
type
  TGameMapType = (mtField, mtTown, mtGuild);
  TGameGUID = Integer;
  GameGuidArray  = array[0..$effffff] of TGameGUID;
  PGameGuidArray = ^GameGuidArray;
  {$IFDEF swserver}
  TGameChar = AnsiChar;
  {$ELSE}
  TGameChar = Byte;
  {$ENDIF}

  TGamePos = record
    x, y, z: Single;

    function getDist(p2: TGamePos): Single;
    function isEqual(p2: TGamePos): Boolean;
  end;
  PGamePos = ^TGamePos;

  TGameMapPos = record
    x, y: Integer;
  end;
  PGameMapPos = ^TGameMapPos;

  TGameName = array[0..19] of TGameChar;
  TGameNameW = array[0..9] of Char;
  TGameNameByte = array[0..19] of Byte;
  TPlayerName = array[0..15] of TGameChar;

  TGameMenPai = (mpNone, mpDT, mpHS, mpFC, mpTM, mpTG, mpPT, mpLG, mpWZ, mpST,
    mpMW, mpYM, mpPS);

  {$IFNDEF gameclient}
  TGameAttributeData = array[0..MaxAttribute] of Single;
  {$ELSE}
  TGameAttributeData = array[0..MaxAttribute] of Word;
  {$ENDIF}
//    _str, _agi, _vit, _int, _dex, _luk,
//    _atk, _matk, _def, _mdef: Integer;

  TCharData = packed record
    _guid: TGameGUID;
    _job: Byte;
    _id: Word;
    _uno: Byte;
    _lev, _race: Byte;
    _pai: TGameMenPai;
    _name: TGameName;
  end;

  TItemType = (itPortion, itPortionLT, itCook, itUseable, itEquip, itAnqi, itPetEquip, itEtc);
  TBuffType = (bfNone, bfGood, bfBad, bfEquip, bfTjGood, bfTjBad);
  TDebuffType = (dbNone, dbWine, dbPoison, dbFeng, dbProp);

const
  wxLei = $20;
  wxTu = $10;
  wxHuo = 8;
  wxShui = 4;
  wxMu = 2;
  wxJin = 1;
  wxLeiHuo = $28;
  wxShuiTu = $14;

  bfPKLock = 10001;
  bfPKProtect = 10002;

type
  TBattleProp = (bpHp, bpMp, bpSP, bpWp, bpAtk, bpDef, bpSpd, bpSpr, bpMAtk, bpMDef, bpHit, bpDodge, bpMDodge, bpWeapon);
  TBattleProps = array[TBattleProp] of SmallInt;
  TBattlePropArray = array[TBattleProp] of Single;
  TDmgArray = array[bpHp..bpWp] of Single;
  TVarsArray = array[0..5] of Single;
  TVarsIntArray = array[0..5] of Integer;

  TGameBuff = packed record
    _id: Integer;
    _type: TBuffType;
    _debuffType: TDebuffType;
    _skId, _deltaTime, _index: Integer;
    _endTime, _endTime2: Integer;
    _srcGuid: TGameGUID;
    _skLev, _maxCnt: Byte;
    _perCri, _perDblHit: Integer;
    _addPropsK: TBattlePropArray; //array[0..TBattleProp] of Single;
    _addProps: TBattleProps;
    _dmgs: TDmgArray;
    _dmgLvs: TDmgArray;
    _dmgK, _phyDmgK, _mDmgK: Single;
    _hurtK, _phyHurtK, _mHurtK: Single;
    _chance: Single;
    _vars: TVarsArray;
    _varInts: TVarsIntArray;
//    _name: TGameName;
//    case Integer of
//      0: (_vars: array[0..5] of Single);
//      1: (_varInts: array[0..5] of Integer);
  end;
  PGameBuff = ^TGameBuff;
  TGameBuffs = array[0..15] of TGameBuff;

  TGameBuffInfo = packed record
    _id: Integer;
    _type: TBuffType;
    _debuffType: TDebuffType;
    _skId, _deltaTime, _index: Integer;
    _endTime, _endTime2: Integer;
    _srcGuid: TGameGUID;
    _skLev, _maxCnt: Byte;
    _perCri, _perDblHit: Integer;
    _addPropsK: TBattlePropArray;
    _addProps: TBattleProps;
    _dmgs: TDmgArray;
    _dmgLvs: TDmgArray;
    _dmgK, _phyDmgK, _mDmgK: Single;
    _hurtK, _phyHurtK, _mHurtK: Single;
    _chance: Single;
    _vars: TVarsArray;
    _varInts: TVarsIntArray;
    _name: string;
//    case Integer of
//      0: (_vars: array[0..5] of Single);
//      1: (_varInts: array[0..5] of Integer);
  end;
  PGameBuffInfo = ^TGameBuffInfo;

  TGameItemContainer = (icBackpack, icEquip, icStorage);
  TGameItemBuff = array[0..3] of Integer;
const
  mpId = 0;
  mpGuid = 1;
  mpSkill = 2;
  mpAccessoryGlow = 15;
type
  TGameItemVars = array[0..15] of Integer;
  TGameStone = packed record
    _id: Word;
    _lev: Byte;
  end;
  TMapPos = packed record
    _mapId, _x, _y: Integer;
    _name: string;
  end;

  {$IFNDEF gameclient}
  TMapPosList = array[1..9] of TMapPos;
const
  AddAttrsValues: array[1..12] of array[0..1] of Integer = (
    (4, 0), (8, -2), (12, -2), (16, -4), (20, -4), (24, -4),
    (28, -6), (32, -6), (36, -8), (42, -8), (46, -10), (48, -12) );
  EnhAttrValues: array[8..12] of Integer = (12, 13, 16, 18, 20);
  EqVarMowangfuhua = 0;
  EqVarAoshulianhua = 2;
  EqVarLongwei = 4;
  EqVarSiwu = 6;
  EqVarShenxingfu = 8;
  EqVarGuaxiang = 10;
  EqVarTjTxChg = 12;
type
  TAddAttrs = array[atVit..atMp] of SmallInt;
  TWxXishous = array[1..6] of Integer;
  TUseJobs = array[0..18] of Boolean;
  TTexiaos = array[0..MaxEqTexiaoCount - 1] of Integer;
  TTejis = array[0..MaxEqTejiCount - 1] of Integer;
  TGameStones = array[0..1] of TGameStone;
  TGameFumo = packed record
    _value: Integer;
    _expireTime: Cardinal;
  end;
  TGameFumos = packed record
    _fmHp, _fmMp, _fmAtk, _fmDef, _fmSpd: TGameFumo;
  end;
  PGameFumos = ^TGameFumos;
  TGameItem = packed record
    _guid: TGameGUID;
    _index, _id, _amount, _lev, _idx: Integer;
    _bind: Word;
    _price, _stack: Integer;

//    _exProp: TGameItemBuff;
    _name: string;

    _type: TItemType;
//    case TItemType of
//    itUseable: (
      _pinzhi: word;
      _hp, _mp, _sp, _wp: Integer;
      _hplev, _mplev, _wplev, _hppz, _mppz, _wppz: Single;
      _buff, _buffTime: Integer;
      _inBattle, _outBattle, _selfUse: Boolean;
//      case Byte of
//        0: (
          _vars: TGameItemVars;
//        1: (
          _mapPosList: TMapPosList;

//      );
//    itEquip: (
      _addAttr: TAddAttrs;
      _ehAddAttr: SmallInt;
      _addProps: TBattleProps;
      _wxXishou: TWxXishous;
      _dur, _maxDur: SmallInt;
      _repairCount, _renZheng: Byte;
      _eqType: Integer;
      _olType, _olIdx: Word;
      _useJobs: TUseJobs;
      _useLev, _useSex: Byte;
      _texiao: TTexiaos;
      _teji: TTejis;
      _soul: Byte;
      _stone: TGameStones;
      _ehMp: SmallInt;
//      _reserved: array[0..8] of Byte);
//    _speed: Single;
  public
    procedure clear;
  end;
  {$ELSE}
  TGameItem = packed record
    _guid: TGameGUID;
    _index, _id, _amount, _lev, _idx: Integer;
    _bind: Word;
    _price, _stack: Integer;
    _ownerIdx: Byte;

//    _exProp: TGameItemBuff;
    _name: string;

    _type: TItemType;
    _info, _infoEx: string;
  public
    procedure clear;
  end;
  {$ENDIF}
  PGameItem = ^TGameItem;
  PPGameItem = ^PGameItem;

  TGameItemLinkInfo = packed record
    _guid: TGameGUID;
    _bind: Integer;
    _infoStr: string;
  end;

  TGameMarketItem = packed record
    _type, _id, _buyPrice0, _buyPrice, _sellPrice, _sellPrice0, _uno: Integer;
    _itmId, _lev: Integer;
    _vars: TGameItemVars;
    _name: string;
  end;
  PGameMarketItem = ^TGameMarketItem;

  T2Bytes = array[0..1] of Byte;
  T3Bytes = array[0..2] of Byte;
  T5Bytes = array[0..4] of Byte;
  T8Bytes = array[0..7] of Byte;
  T12Bytes = array[0..11] of Byte;
  TPeishi = array[0..2] of array[0..1] of Byte;
  TGameAppearance = packed record
    _color: T3Bytes;
    _weapon1: T2Bytes;
    _weapon2: T3Bytes;
    _uno3: Byte;
    _uno4: Word;
    _uno5: T3Bytes;
    _mount: T12Bytes;  //类型,装饰,地面,颜色,5?,6?,装饰色,8?,9?,地面色,11?,12?
    //7,8 0byte
    _spirit1: Byte;
    _flower: Byte;
    _mountProp: T8Bytes;
    _peishi: TPeishi;
    _shouHuShou: T5Bytes;
    _spirit2: T2Bytes;
  end;
  PGameAppearance = ^TGameAppearance;

  TGamePlayerFaceData = array[1..12] of Byte;

  TGameFriendData = packed record
    _guid: TGameGUID;
    _job, _lev: Byte;
    _pai: TGameMenPai;
    _temporary, _block: Boolean;
    _youhaodu: Word;
    _relation, _group, _stat, _mobile: Byte;
    _name, _comment: string;
    _player: Pointer;
    _msgCnt: Byte;
  end;
  PGameFriendData = ^TGameFriendData;

  TGamePlayerEquips = array[1..MaxEquipCount + MaxZhuangshiCount] of TGameItem;
  TGamePlayerEquipsList = array[1..MaxEquipCount + MaxZhuangshiCount] of PGameItem;
  TGamePlayerBackPack = array[0..MaxBackpackSize - 1] of TGameItem;
  TGamePlayerBackPackList = array[0..MaxBackpackSize - 1] of PGameItem;
  TGamePlayerTradePack = array[0..MaxTradepackSize - 1] of TGameItem;
  TGamePlayerTradePackList = array[0..1] of TGamePlayerTradePack;
  TGamePlayerStoragePack = array[0..MaxStorageSize - 1] of TGameItem;
  TGamePlayerStoragePackList = array[-1..MaxStorageSize] of PGameItem;
  TGamePlayerFriends = array[0..MaxFriendCount - 1] of TGameFriendData;

  TGamePetEquips = array[1..MaxPetEquipCount] of TGameItem;


  TGameObjPropData = packed record
    _guid: TGameGUID;
    _id, _job, _race, _lev, _exp, _rexp,
    _hp, _mp, _wp: Integer;
    _pai: TGameMenPai;
    _maxHp, _maxMp: Single;
    _sp, _maxSp: Integer;
    _attribute: TGameAttributeData;
    _spd, _atkSpd: Single;
    _facing: Byte;

    _mapId, _sceneId: Integer;
    _pos, _tarPos: TGamePos;

    _gold: Integer;

//    unoData: array[0..$4F] of byte;
  end;

  TDailyStatuses = array[0..MaxPlayerDailyStatusCount - 1] of Integer;
  TMonthlyStatuses = array[0..MaxPlayerMonthlyStatusCount - 1] of Integer;
  TStatuses = array[0..MaxPlayerStatusCount - 1] of integer;
  TTitleData = packed record
    _id: Word;
    _uno1, _clr, _uno2: Byte;
    _info: string;
    _uno3: Byte;
    _text: string;

    procedure _init;
  end;
  TTitleDataPair = TPair<Word,TTitleData>;

  TGamePlayerProp = packed record
    _bindGold, _xyb, _Jinbi, _arenaPoint, _fxq: Integer;
    _huoli, _maxHL, _tili, _maxTili, _renqi, _yinde: Integer;
    _maxPetCount, _maxSummonCount: Byte;
    _reverse0: Word;
    _activePetGuid, _activeMountGuid: TGameGUID;
    _faceData: TGamePlayerFaceData;
    _dailyStatus: TDailyStatuses;
    _monthlyStatus: TMonthlyStatuses;
    _status: TStatuses;
    _titleId: Word;
    _error: Boolean;
  end;
  TJinbiAction = (jaNone, jaBuyPet, jaChangeName, jaRepaiRequip, jaCreateGuild,
    jaShopBuyItem, jaMallBuyItem, jaBuyBackpackSlot, jaBuyStorageSlots, jaRybyCompose,
    jaCharge);
  TGameJibiOption = record
    _action: TJinbiAction;
    _amt: Integer;
    _v1, _v2: Integer;
    _s1, _s2: string;
    _guids: array[0..15] of TGameGUID;
  end;

  TPetZz = (zzAtk, zzDef, zzMp, zzHp, zzSpd);
  TGamePetZzs = array[TPetZz] of Integer;
  TGameMountAppearance = packed record
    _accessory, _glow: Byte;
    _clr1, _clr2, _uno1: Byte;
    _accClr: Byte;
    _uno2: Word;
    _glowClr: Byte;
    _uno3: Word;
  end;
  TGamePetProp = packed record
    _life, _talent, _useLevel, _loyalty, _type, _maxSkCnt, _jxCnt: Integer;
//    case Integer of
//    0:(
    _petZzs, _petZzsMaxNow, _petZzsMax: TGamePetZzs;
    _growing: Single;
    _growingRate: Integer;
    _bind: Boolean;
//    1:(
    _accessory, _glow: Boolean;
    _mountApp: TGameMountAppearance;
  end;

  TSkillType = (stNone, stPassive, stPhysicalSpell, stGoodSpell, stBadSpell, stFeng,
    stPhysicalTeji, stMagicalTeji, stGoodTeji, stBadTeji);

  TDmgType = (dtNone, dtHp, dtMp, dtWp, dtSp);
  TDmgTypes = set of TDmgType;

  TSkillDataType = (sdNone, sdJichuFuzhu, sdShimen, sd3, sdTeji);

  PGameSkillData = ^TGameSkillData;
  TGameSkillData = packed record
    _id: Integer;
    _lev, _pwr, _maxPwr: Byte;
    _type: TSkillDataType;
    _cdTime: Int64;

    _isEqSkill, _bind: Boolean;
    _parent: PGameSkillData;
    {$IFDEF gameclient}
    _uno1, _useType: Byte;
    {$ENDIF}
  end;

  TGamePlayerSkills = array[0..MaxSkillCount - 1] of TGameSkillData;
  TGamePetSkills = array[0..MaxPetSkillCount - 1] of TGameSkillData;

  TGameShortCut = packed record
    _type, _id: Integer;
  end;
  TGameShortCutData = array[0..31] of TGameShortCut;

  TGameQuestType = (qtJuqing, qtLilian, qtShimen, qtZhuogui, qtYunbiao);
  TGameQuestText = array[0..255] of TGameChar;
  TGameQuestTarget = packed record
    _id, _pwr, _amt: Integer;
  end;
  TGameQuestTargetList = array[0..9] of TGameQuestTarget;
  TGameQuestTargetType = (ttTalk, ttBattle, ttKill, ttCollect);
  TGameQuestFinish = array[0..2] of Byte;
  TQuestTitle = array[0..27] of TGameChar;
  TGameQuestData = packed record
    _id: Integer;
    uno1: Byte;
    uno2: Integer;
    _title: TQuestTitle;
    _finish: TGameQuestFinish;
    _expireTime: Integer;
    _rwmb, _rwhf, _rwgl, _rwms, _rwxx: TGameQuestText;
    _pubflag, _qnpcGuid: Integer;
    _qnpcName: TGameName;
    _qnpcId: Word;
    _qnpcApp: TGameAppearance;
    _qmapId: Word;
    _qmapPos: TGamePos;
    _qnpcFacing: Byte;
    _qmark: Boolean;
    _qnpcEmote: Byte;
    _husong: Boolean;
    _qnpcTitle: TGameName;
    _uno3: Word;

    //----------------
    _type: TGameQuestType;
    _tgType: TGameQuestTargetType;
    _tgList: TGameQuestTargetList;
    _itemList: TGameQuestTargetList;
    _bonusExp, _bonusGold, _bonusBindGold: Integer;
    _bonusItems: TGameQuestTargetList;
    _nextQuestId: Integer;
  end;
  PGameQuestData = ^TGameQuestData;
  TGameQuest = packed record
    _id: Integer;
    _flag: Word;
    _expireTime: Int64;
    _finish: TGameQuestFinish;
//    _title: TQuestTitle;
    {$IFNDEF gameclient}
    _qnpcName, _qscript: string;
    {$ENDIF}
    _qnpcId: Word;
//    _qnpcApp: TGameAppearance;
    _qmapId: Word;
    _qmapPos: TGamePos;
    _qnpcFacing: Byte;
    _tgList: TGameQuestTargetList;
    _bonusK: Single;
    {$IFDEF gameclient}
    _title, _rwmb, _rwhf, _rwgl, _rwms, _rwxx: string;
    _pubflag, _qnpcGuid: Integer;
    _qnpcName: string;
    _qnpcApp: TGameAppearance;
    _qmark: Boolean;
    _qnpcEmote: Byte;
    _husong: Boolean;
    _qnpcTitle: string;
    {$ENDIF}
  end;
  PGameQuest = ^TGameQuest;
  TGameQuestDatas = array[0..MaxQuestCount - 1] of TGameQuest;
  TGameQuestList = array[0..MaxQuestCount - 1] of PGameQuest;

  TDbActionType = (datNone,
    datAccLogin,
    datAccLoginSucc,
    datAccLoginFail,
    datAccGetCharList,
    datAccGetCharListSucc,
    datAccJinbiCharge,
    datAccJinbiChargeSucc,
    datAccJinbiChargeFail,
    datAccJinbiUse,
    datAccJinbiUseSucc,
    datAccJinbiUseFail,
    datAccSwbiChange,
    datSaveAcc,
    datSaveChar,
    datSaveCharSucc,
    datSaveCharFail,
    datLoadChar,
    datLoadCharSucc,
    datLoadCharFail,
    datCreateChar,
    datCreateCharSucc,
    datCreateCharFail,
    datChangeName,
    datChangeNameSucc,
    datChangeNameFail,
    datChangeProp,
    datCreateGuild,
    datCreateGuildSucc,
    datCreateGuildFail,
    datLoadGuild,
    datSaveGuild,
    datLoadAllGuild,
    datSaveGuildSucc,
    datExitServer);
  TDbAction = class
  public
    FType: TDbActionType;
    obj: Pointer;
    class function DbActionTypeToString(dat: TDbActionType): string;
  end;

function GetGamePos(x, y, z: Single): TGamePos;
function GetDistance(p1, p2: TGamePos): Single; overload;
function GetDistance(x1, y1, x2, y2: Single): Single; overload;
function GetNextPos(p1, p2: TGamePos; dis: Single): TGamePos;
function ApproachTo(p1, p2: TGamePos; dis: Single): TGamePos;
function GetRandomPos(p0: TGamePos; r: Integer): TGamePos;
function makeWord(a, b: Byte): Word;
function getDirection(p1, p2: TGamePos): Byte;

procedure GameStringToCharArray(var s: string; p: Pointer; len: Integer = 20);
function GameCharArrayToString(p: Pointer; len: Integer = 20): string;

procedure GameIncVar(var iVar: Integer; AValue, AMin, AMax: Integer);

procedure GetRandomAttr(attr1, attr2, cnt: Integer; var alist: array of Integer);

function IndexToGameItemIndex(idx: Integer): Integer;
function GameItemIndexToIndex(idx: Integer): Integer;

type
  TGameMemoryStream = class(TMeiStream)
  public
    function ReadGameGUID: TGameGUID;
    function ReadGamePos: TGamePos;
    function readAppearance(var app: TGameAppearance): Integer;
    procedure WriteGameGUID(guid: TGameGUID);
    procedure WriteGamePos(p: TGamePos);
    procedure WriteGameString(s: string; withSize: Boolean = False; nullEnd: Boolean = False);
    procedure WriteGameName(name: TGameName);
    procedure WriteGameNameString(s: string);
    procedure WriteGameNameToAnsiString(name: TGameName);
    procedure WritePlayerName(name: TPlayerName);
    procedure WritePlayerNameString(s: string);
    {$IFDEF swserver}
    procedure WriteGameAppearance(gobj: Pointer);
    procedure WriteGameAppearanceData(p: Pointer);
    function ReadGameString(sizeSize: Integer = 0): AnsiString;
    function ReadGameStringWithSize(size: Integer): AnsiString;
    function ReadGameName: TGameName;
    procedure loadFromString(s: AnsiString);
    {$ELSE}
    function ReadGameName: string;
    function ReadGameString(sizeSize: Integer = 0): string;
    function ReadGameStringWithSize(size: Integer): string;

    {$ENDIF}
    function ReadGameCount(maxCnt: Integer = 1000): Integer;
    function getHexText(len: Int64; offset: Int64 = 0): string;

    procedure lock;
    procedure unlock;

//    procedure InitPacket(cmd, subCmd: Byte);

  end;

implementation

uses
  System.Math{$IFDEF SWSERVER}, ObjUnit{$ENDIF}, System.SysUtils, System.TypInfo;

function GetGamePos(x, y, z: Single): TGamePos;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;

function GetDistance(p1, p2: TGamePos): Single;
begin
  Result := Sqrt(Sqr(p2.x - p1.x) + Sqr(p2.y - p1.y));
end;

function GetDistance(x1, y1, x2, y2: Single): Single; overload;
begin
  Result := Sqrt(Sqr(x2 - x1) + Sqr(y2 - y1));
end;

function GetNextPos(p1, p2: TGamePos; dis: Single): TGamePos;
var
  k, d: Single;
begin
  d := GetDistance(p1, p2);
  if (d < 0.1) or (d < dis) then
  begin
    Result := p2;
    Exit;
  end;

  k := dis/d;
  Result.x := (p2.x - p1.x) * k + p1.x;
  Result.y := (p2.y - p1.y) * k + p1.y;
  Result.z := (p2.z - p1.z) * k + p1.z;
end;

function ApproachTo(p1, p2: TGamePos; dis: Single): TGamePos;
var
  k, d: Single;
begin
  d := GetDistance(p1, p2);

  if (d < dis) then
  begin
    Result := p1;
    Exit;
  end;

  k := (d - dis - 0.01)/d;
  Result.x := (p2.x - p1.x) * k + p1.x;
  Result.y := (p2.y - p1.y) * k + p1.y;
  Result.z := (p2.z - p1.z) * k + p1.z;
end;

function GetRandomPos(p0: TGamePos; r: Integer): TGamePos;
begin
  Result.x := p0.x - r + Random(r * 2);
  Result.y := p0.y - r + Random(r * 2);
  Result.z := p0.z;
end;

function makeWord(a, b: Byte): Word;
begin
  Result := A or B shl 8;
end;

function getDirection(p1, p2: TGamePos): Byte;
var
  a: Single;
  ideg: Integer;
begin
  if p2.x <> p1.x then
    a := ArcTan2(p2.y - p1.y, p2.X - p1.x)
  else if p2.y > p1.y then
    a := Pi / 2
  else
    a := - Pi / 2;
  a := a + Pi / 4;
  if a < 0 then
    a := Pi * 2 + a;
  ideg := Trunc(a / (Pi * 2 / 360));
  Result := Trunc(a / Pi * 4);
  if a > Result * Pi / 4 + Pi / 8 then
    Inc(Result);
  Result := (8 - Result) and 7;
end;

procedure GameStringToCharArray(var s: string; p: Pointer; len: Integer = 20);
var
  a, b: TBytes;
begin
  FillChar(p^, len, 0);
  if s = '' then
    Exit;
  b := BytesOf(s);
  a := TEncoding.Convert(TEncoding.Default, GameEncoding.ANSI, b);
  Move(a[0], p^, Min(len, Length(a)));
  SetLength(a, 0);
  SetLength(b, 0);
end;

function GameCharArrayToString(p: Pointer; len: Integer = 20): string;
var
  gname: TGameName;
  a, b: TBytes;
  pb: PByte;
  i: Integer;
begin
  pb := p;
  for i := 0 to len - 1 do
  begin
    if pb^ = 0 then
    begin
      len := i;
      Break;
    end;
    Inc(pb);
  end;
  if len = 0 then
  begin
    Result := '';
    Exit;
  end;

  SetLength(a, len);
  Move(p^, a[0], len);
  b := TEncoding.Convert(GameEncoding.ANSI, TEncoding.Default, a);
  Result := StringOf(b);
  SetLength(b, 0);
  SetLength(a, 0);
end;


procedure GameIncVar(var iVar: Integer; AValue, AMin, AMax: Integer);
begin
  Inc(iVar, AValue);
  if iVar < AMin then
    iVar := AMin;
  if iVar > AMax then
    iVar := AMax;
end;

procedure GetRandomAttr(attr1, attr2, cnt: Integer; var alist: array of Integer);
var
  abc: array[0..15] of Integer;
  acnt, k, idx, i: Integer;
begin
  acnt := attr2 - attr1 + 1;
  for i := 1 to cnt do
  begin
    if i mod acnt = 1 then
      for k := attr1 to attr2 do
        abc[k - attr1] := k;
    idx := Random(cnt);
    for k := 0 to attr2 - attr1 do
    if abc[k] >= 0 then
    begin
      if idx = 0 then
      begin
        Dec(cnt);
        alist[cnt] := abc[k];
        abc[k] := -1;
        if cnt = 0 then
          Exit;
        Break;
      end;
      Dec(idx);
    end;
  end;
end;

function IndexToGameItemIndex(idx: Integer): Integer;
const
  itmIdxs: array[0..69] of Integer = (
    20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
    100, 101, 102, 103, 104, 105, 106, 107, 108, 109,
    50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79
  );
begin
  if InRange(idx, 0, 59) then
    Result := itmIdxs[idx]
  else
    Result := -1;
end;

function GameItemIndexToIndex(idx: Integer): Integer;
const
  itmIdxs: array[20..109] of Integer = (
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
    40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39
    );
begin
  if InRange(idx, 20, 109) then
    Result := itmIdxs[idx]
  else
    Result := -1;
end;


{ TGameMemoryStream }

//procedure TGameMemoryStream.InitPacket(cmd, subCmd: Byte);
//begin
//end;

function TGameMemoryStream.readAppearance(var app: TGameAppearance): Integer;
var
  idx, k, cnt: Integer;
begin
  Result := Position;
  FillChar(app, SizeOf(app), 0);
  for k := 1 to ReadByte do
  begin
    idx := ReadByte;
    case idx of
      0:
        Read(app._color, 3);
      1:
        Read(app._weapon1, 2);
      2:
        Read(app._weapon2, 3);
      3:
        Read(app._uno3, 1);
      4:
        Read(app._uno4, 2);
      5:
        Read(app._uno5, 3);
      6:
        Read(app._mount, 12);
      9:
        Read(app._spirit1, 1);
      10:
        Read(app._flower, 1);
      11:
        Read(app._mountProp, 8);
      12..14:
        Read(app._peishi[idx - 12], 2);
      15:
        Read(app._shouHuShou, 5);
      16:
        Read(app._spirit2, 2);
    end;
  end;
  Result := Position - Result;
end;

function TGameMemoryStream.ReadGameCount(maxCnt: Integer): Integer;
begin
  Result := 0;
  Read(Result, SizeOf(Result));
  if Result > maxCnt then
    raise Exception.Create('ReadGameCount Overflow, maxCnt = ' + maxCnt.ToString);
end;

function TGameMemoryStream.ReadGameGUID: TGameGUID;
begin
  Result := 0;
  Read(Result, SizeOf(TGameGUID))
end;

function TGameMemoryStream.ReadGamePos: TGamePos;
begin
  Result.x := ReadWord;
  Result.y := ReadWord;
  Result.z := 0;
end;

function TGameMemoryStream.getHexText(len, offset: Int64): string;
var
  p: int64;
begin
  p := Position + offset;
  if len < 0 then
    len := Size - p
  else if len > Size - Position then
    len := Size - p;

//  Result := BufToHexText(Memories[p], len);
end;

procedure TGameMemoryStream.lock;
begin
  TMonitor.Enter(Self);
end;

procedure TGameMemoryStream.unlock;
begin
  TMonitor.Exit(Self);
end;

{$IFDEF swserver}
procedure TGameMemoryStream.loadFromString(s: AnsiString);
begin
  SetSize(0);
  WriteAnsiString(s);
  Seek(0, 0);
end;

function TGameMemoryStream.ReadGameName: TGameName;
begin
  Read(Result, SizeOf(Result));
end;

procedure TGameMemoryStream.WriteGameAppearance;
var
  pb: PByte;
  cnt: Integer;
  p: TGameObj;
  i: Integer;
begin
  WriteByte(0);
  pb := CurrentMem;
  cnt := 0;
  p := gobj;

  if PInteger(@p.FAppearance._color[0])^ and $FFFFFF <> 0 then
  begin
    WriteByte(0);
    Write(p.FAppearance._color, 3);
    Inc(cnt);
  end;

  if PWord(@p.FAppearance._weapon2[0])^ <> 0 then
  begin
    WriteByte(2);
    Write(p.FAppearance._weapon2, 3);
    Inc(cnt);
  end else
  if p.FAppearance._weapon1[0] <> 0 then
  begin
    WriteByte(1);
    Write(p.FAppearance._weapon1, 2);
    Inc(cnt);
  end;

  if (p.FType and otPlayer <> 0) or (p.FAppearance._uno3 <> 0) then
//  if (p.FAppearance._uno3 <> 0) then
  begin
    WriteByte(3);
    WriteByte(p.FAppearance._uno3);
    Inc(cnt);
  end;

  if p.FAppearance._uno4 <> 0 then
  begin
    WriteByte(4);
    Write(p.FAppearance._uno4, 2);
    Inc(cnt);
  end;

//  if p.FAppearance._uno5 <> 0 then
//  begin
//    WriteByte(3);
//    WriteByte(p.FAppearance._uno3);
//    Inc(cnt);
//  end;

  if p.FAppearance._mount[0] <> 0 then
  begin
    WriteByte(6);
    Write(p.FAppearance._mount, 12);
    Inc(cnt);
  end;

//7,8 0 byte

  if p.FAppearance._spirit1 <> 0 then
  begin
    WriteByte(9);
    WriteByte(p.FAppearance._spirit1);
    Inc(cnt);
  end;

  if p.FAppearance._flower <> 0 then
  begin
    WriteByte(10);
    WriteByte(p.FAppearance._flower);
    Inc(cnt);
  end;

  if p.FAppearance._mountProp[0] <> 0 then
  begin
    WriteByte(11);
    Write(p.FAppearance._mountProp, 8);
    Inc(cnt);
  end;

  for i := 0 to 2 do
  if InRange(p.FAppearance._peishi[i, 0], 2, 4) then
//    InRange(p.FAppearance._peishi[i, 1], 1, 10) then
  begin
    WriteByte(12 + i);
    Write(p.FAppearance._peishi[i], 2);
    Inc(cnt);
  end;

  if p.FAppearance._shouHuShou[0] <> 0 then
  begin
    WriteByte(15);
    Write(p.FAppearance._shouHuShou, 5);
    Inc(cnt);
  end;

//  if p.FAppearance._spirit2[0] <> 0 then
//  begin
//    WriteByte(16);
//    Write(p.FAppearance._spirit2, 2);
//    Inc(cnt);
//  end;

  pb^ := cnt;
end;
procedure TGameMemoryStream.WriteGameAppearanceData(p: Pointer);
var
  app: PGameAppearance;
  pb: PByte;
  cnt: Integer;
  i: Integer;
begin
  if p = nil then
    Exit;
  app := p;
  WriteByte(0);
  pb := CurrentMem;
  cnt := 0;

  if PInteger(@app._color[0])^ and $FFFFFF <> 0 then
  begin
    WriteByte(0);
    Write(app._color, 3);
    Inc(cnt);
  end;

  if PWord(@app._weapon2[0])^ <> 0 then
  begin
    WriteByte(2);
    Write(app._weapon2, 3);
    Inc(cnt);
  end else
  if app._weapon1[0] <> 0 then
  begin
    WriteByte(1);
    Write(app._weapon1, 2);
    Inc(cnt);
  end;

  if (app._uno3 <> 0) then
//  if (app._uno3 <> 0) then
  begin
    WriteByte(3);
    WriteByte(app._uno3);
    Inc(cnt);
  end;

  if app._uno4 <> 0 then
  begin
    WriteByte(4);
    Write(app._uno4, 2);
    Inc(cnt);
  end;

//  if app._uno5 <> 0 then
//  begin
//    WriteByte(3);
//    WriteByte(app._uno3);
//    Inc(cnt);
//  end;

  if app._mount[0] <> 0 then
  begin
    WriteByte(6);
    Write(app._mount, 12);
    Inc(cnt);
  end;

//7,8 0 byte

  if app._spirit1 <> 0 then
  begin
    WriteByte(9);
    WriteByte(app._spirit1);
    Inc(cnt);
  end;

  if app._flower <> 0 then
  begin
    WriteByte(10);
    WriteByte(app._flower);
    Inc(cnt);
  end;

  if app._mountProp[0] <> 0 then
  begin
    WriteByte(11);
    Write(app._mountProp, 8);
    Inc(cnt);
  end;

  for i := 0 to 2 do
  if app._peishi[i, 0] <> 0 then
  begin
    WriteByte(12 + i);
    Write(app._peishi[i], 2);
    Inc(cnt);
  end;

  if PWord(@app._shouHuShou[0])^ <> 0 then
  begin
    WriteByte(15);
    Write(app._shouHuShou, 5);
    Inc(cnt);
  end;

  if app._spirit2[0] <> 0 then
  begin
    WriteByte(16);
    Write(app._spirit2, 2);
    Inc(cnt);
  end;

  pb^ := cnt;
end;

function TGameMemoryStream.ReadGameString;
var
  len: Integer;
begin
  if sizeSize = 0 then
  begin
    len := Size - Position;
    Result := ReadAnsiString(len);
  end else
  begin
    len := ReadByte;
    Result := ReadAnsiString(len);
  end;
end;
function TGameMemoryStream.ReadGameStringWithSize(size: Integer): AnsiString;
begin
  if size <= 0 then
    Exit('');
  SetLength(Result, size);
  Read(Result[1], size);
  Result := PAnsiChar(Result);
end;

{$ELSE}
function TGameMemoryStream.ReadGameName: string;
var
  gname: TGameName;
  b: TBytes;
begin
  FillChar(gname, SizeOf(gname), 0);
  Read(gname, SizeOf(gname));
  b := TEncoding.Convert(GameEncoding.ANSI, TEncoding.Default, gname);
  Result := StringOf(b);
  SetLength(b, 0);
  Result := Result.Trim;
end;

function TGameMemoryStream.ReadGameString;
var
  len: Integer;
begin
  if sizeSize = 0 then
  begin
    len := Size - Position;
    Result := ReadAnsiString(len);
  end else
  begin
    len := 0;
    Read(len, sizeSize);
    Result := ReadAnsiString(len);
  end;
end;
function TGameMemoryStream.ReadGameStringWithSize(size: Integer): string;
begin
  Result := string(ReadAnsiString(size));
end;
{$ENDIF}
procedure TGameMemoryStream.WriteGameGUID(guid: TGameGUID);
begin
  Write(guid, SizeOf(TGameGUID));
end;

procedure TGameMemoryStream.WriteGameName(name: TGameName);
begin
  Write(name, SizeOf(name));
end;

procedure TGameMemoryStream.WriteGameNameString(s: string);
var
  gname: TGameName;
begin
  GameStringToCharArray(s, @gname);
  WriteGameName(gname);
end;

procedure TGameMemoryStream.WriteGameNameToAnsiString(name: TGameName);
var
  i: Integer;
begin
  for i := 0 to High(name) do
    if name[i] = tgamechar(0) then
      Break;
  Write(name, i);
end;

procedure TGameMemoryStream.WriteGamePos(p: TGamePos);
begin
  WriteWord(Trunc(p.x));
  WriteWord(Trunc(p.y));
end;

procedure TGameMemoryStream.WriteGameString(s: string; withSize,
  nullEnd: Boolean);
var
  a, b: TBytes;
begin
  if s = '' then
  begin
    if withSize or nullEnd then
      WriteByte(0);
    Exit;
  end;
  a := BytesOf(s);
  b := TEncoding.Convert(TEncoding.Default, GameEncoding.ANSI, a);
  if withSize then
    WriteByte(Length(b));
  Write(b[0], Length(b));
  if nullEnd then
    WriteByte(0);
end;

procedure TGameMemoryStream.WritePlayerName(name: TPlayerName);
begin
  Write(name, SizeOf(name));
end;

procedure TGameMemoryStream.WritePlayerNameString(s: string);
var
  pn: TPlayerName;
begin
  GameStringToCharArray(s, @pn, SizeOf(pn));
  WritePlayerName(pn);
end;

{ TGameItem }
{$IFNDEF gameclient}
procedure TGameItem.clear;
begin
  _name := '';
  FillChar(_guid, SizeOf(TGameItem), 0);
end;
{$ELSE}
procedure TGameItem.clear;
begin
  _guid := 0;
  _id := 0;
  _info := '';
  _infoEx := '';
end;

{$ENDIF}

{ TTitleData }

procedure TTitleData._init;
begin
  FillChar(Self, SizeOf(TTitleData), 0);
  Self._id := tsGuild;
  Self._uno1 := 1;
  Self._clr := 1;
end;

{ TDbAction }

class function TDbAction.DbActionTypeToString(dat: TDbActionType): string;
begin
  Result := GetEnumName(TypeInfo(TDbActionType), Ord(dat));
end;

{ TGamePos }

function TGamePos.getDist(p2: TGamePos): Single;
begin
  Result := Sqrt(Sqr(p2.x - x) + Sqr(p2.y - y));
end;

function TGamePos.isEqual(p2: TGamePos): Boolean;
begin
  Result := getDist(p2) < 0.1;
end;

end.

