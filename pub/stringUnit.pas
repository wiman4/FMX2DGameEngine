unit stringUnit;

interface

uses
  TypeUnit{$ifdef gameclient}, objUnit{$endif};
const
  sPwdErr = '#R帳號密碼錯誤，請注意大小寫！#n';
  sChineseDizhi: array[0..11] of string = (
    '子', '丑', '寅', '卯', '辰', '巳', '午', '未', '申', '酉', '戌', '亥' );
  sChineseNumber: array[0..9] of string = (
    '零', '一', '二', '三', '四', '五', '六', '七', '八', '九');
  sGameTime = '%s时%s刻';
  sUno = '未知.';
  sNotLearned = '【未习得】';
  sPlayer = '玩家';
  MenPaiNames: array[TGameMenPai] of string = (
    '未加入门派', '大唐官府', '化生寺', '方寸山', '天魔里', '天宫', '普陀山',
    '龙宫', '五庄观', '狮驼岭', '魔王寨', '幽冥地府', '盘丝洞' );
  sBaseProps: array[0..7] of string = (
    'I D', '名称', '等级', '门派', '帮派', '称谓', '体力', '活力');
  sAttrs: array[0..5] of string = (
    '体质', '魔力', '力量', '耐力', '敏捷', '潜力');
  sBltProps: array[0..5] of string = (
    '气血', '魔法', '攻击', '防御', '速度', '灵力' );
  sPropLevJob = '%3d级　%s';
  sJobNames: array[0..12] of string = (
    '', '剑侠客', '晓飞燕', '逍遥生', '楚碧秋', '神天兵', '玉临香', '龙太子',
    '凌霜月', '阿修罗', '狐美人', '狮魔王', '沐雨微' );
  sZhenFaName: array[0..MaxZhenfaID] of string = (
    '普通陣', '天覆陣', '地載陣', '風揚陣', '雲垂陣', '龍飛陣', '虎翼陣', '鳥翔陣', '蛇蟠陣' );

  sPetProps: array[0..3] of string = (
    '天資', '攜帶等級', '等級', '忠誠' );
  sPetZZs: array[TPetZz] of string = (
    '攻擊資質', '防禦資質', '體力資質', '法力資質', '速度資質' );

  sNameAndLevel = '%s　(%d级)';

  sSlotEmpty = '空';

  sChatChns: array[0..15] of string = (
    '#cfffbf7[當前]#n', '#cff6500[隊伍]#n', '#c00fb00[私聊]#n', '#c00ffff[幫派]#n',
    '#ce7dff7[地區]#n', '#c0092ff[提示]#n', '#ced00ed[夫妻]#n', '#ccefbed[傳聞]#n',
    '#ced0000[系統]#n', '#cdef7a5[等級]#n', '#ced0000[系統]#n', '#c84c3f7[世界]#n',
    '#ced0000[系統]#n', '#cef6521[組隊]#n', '#cb5a2e7[聯盟]#n', '#cf7be7b[問答]#n' );
  ChatChnColors: array[0..15] of Cardinal = (
    $ffffffff, $ffff6500, $ffffffff, $ff00ffff, $ffffffff, $ffffff00, $ffff00ff,
    $ffffff00, $ffffffff, $ffce9aff, $ffffffff, $ff84c3ff, $ffffff00, $ffef6521 ,
    $ffb5a2e7 , $fff7be84 );
  sColon = '：';

  sMainMenuAttackTips: array[1..2] of string = ( '请选择想要#r攻击#n的目标', '请选择想要#y比试#n的目标' );
  sMainMenuTradeTips: array[1..2] of string = ( '请选择想要#g交易#n的目标', '请选择想要#y给予#n的目标' );
  sMainMenuPartyTips = '请选择想要#g邀请组队#n的目标';


  sBattleStart = '进入战斗';
  sAttack = '普攻';
  sReady = '就绪';
  sDodge = '闪避';
  sParry = '招架';
  sCrit = '！';
  sDead = '死亡';
  sOut = '场外';
  sFanjiLianjiZhuiji = '反击连击追击';
  sChooseAtkTarget = '请选择想要攻击的敌方目标';
  sChooseEnemy = '请选择一个敌方目标';
  sChooseAlias = '请选择一个友方目标';
  sSkillTargetErr = '无法对该对象使用此技能！';
  sAttackTargetErr = '无法攻击友军！！';
  sNoSkillId = '请先选择想要使用的技能！';
  sNoItemId = '请先选择想要使用的物品！';
  sCaptureTargetErr = '无法对该对象使用捕捉！';

  sInBattle = '战斗内使用';
  sMedicine = '级药#r【';

  sNoPlayerSelected = '請先選擇一個玩家';
  sNoParty = '你没有队伍……';

  sItemPrice = '價:%s';
  sEquipTypes: array[0..15] of string = (
    '朴刀', '利剑', '枪戟', '折扇', '长鞭', '短棒', '法珠', '双环',
    '男帽', '男衣', '男鞋',
    '女帽', '女衣', '女鞋',
    '项链', '腰带' );
  sEquipComposeTypes: array[0..2] of string = (
    '普通制造', '强化制造', '强化重铸' );
  sLev = '%d級';
  sNameAmount = '%s×%d';
  sEqComponent1 = '[%d級%s之靈] %d/1'#13'註：神武幣交易中心購買';
  sEqComponent1Name = '%d级%s之灵';
  sEqComponent2: array[0..2] of string = (
    '%d級[%s]或[裝備模具] %d/1'#13'註：怪物掉落或冶煉店老闆購買',
    '%d級製造[%s]'#13'%d級製造[%s]'#13'註：鐵匠普通製造',
    '%d級強化製造[%s] %d/1'#13'註：鐵匠強化製造' );
  sEqComponent3 = '%d級以上[打造符] %d/1'#13'註：冶煉店老闆或攤位購買';
  sEqComposeTili = '所需體力：%d/%d';
  sItemKeyFmt = '【%s】';
  sItemLevel = '【等级】';
  sItemMaker = '【制造人】';
  sItemType = '【类型】';
  sItemJob = '【适用角色】';
  sItemMakeLev: array[0..2] of string = ('', '系统打造', '系统强化打造');
  sItemNotFound = '背包裡沒有足夠的物品：%s';
  sItemDeleteConfirm = '#R要删除#Y[%s]#n？';

  sCostGold = '所需金錢：%d/%d';
  sCostBindGold = '所需信譽：%d/%d';
  sCostJB = '所需金幣：%d/%d';
  sCostSWB = '所需神武幣：%d/%d';
  sRepairUseJB = '花費%d金幣代替修理符';
  sInsufficientGold = '現金不足！';
  sInsufficientBindGold = '信譽不足！';

  sPartyCreateConfirm = '您當前沒有隊伍，是否創建？';
  sPartyCreateSucc = '創建隊伍成功！';
  sPartyInvite = '想邀請您加入隊伍，您同意麼？';
  sPartyMemberName = '%s(%s)';
  {$IFDEF gameclient}
  sPartyStatus: array[TPartyMemberStatus] of string = (
    '正常', '暫離', '離線' );
  {$ENDIF}

  sMallPrice = '元宝：%d';

  sNoMarketItemSelected = '請先選擇需要購買的物品';

  sPetChgNamePrompt = '请输入新的名字：';
  sDropPetErr1 = '放生前请先将宠物名字改回默认名字“%s”';
  sDropPetErr2 = '参战状态的宠物不能放生';
  sNoFocusPet = '請先選擇要操作的寵物！';
  sChooseLianhuaZZ = '請選擇一項副寵的資質煉化';
  sPetLianhuaResult = '骰子投擲結果為：#R%d#n，%s煉化結果為：#R%d#n。#Y%s現在的%s為：#R%d#n';
  sPetXibaobaoConfirm = '%s 等級大於10級，請先到長安城萬獸通處提交洗寵申請！';

  sTradeLock: array[Boolean] of string = ('點擊鎖定', '本方已鎖定');
  sTradeLock2: array[Boolean] of string = ('對方未鎖定', '對方已鎖定');
  sTradeSucc = '交易成功！';
  sTradeUnlock = '雙方鎖定之後才可以確認交易！';

  sExitGame = '#R是否接受退出#Y神武西游2#n的操作？';
  sRelogin = '#Y與游戲服務器連接已斷開，是否重新登錄？';

  sSkillStudyExp = '所需經驗：%d/%d';
  sSkillStudyBindGold = '所需信譽：%d/%d';

  sRWMB = '#Y任務目標：#n';
  sRWHF = '#Y任務回覆：#n';
  sRWGL = '#Y任務攻略：#n';
  SRWMS = '#Y任務描述：#n';
  sQuestMapErr = '#R任務目標不在當前地圖！#n';

  sTargetUnreachable = '目標不可移動！';
  sAutoTalk = '自動尋路交談：%s';

implementation

end.
