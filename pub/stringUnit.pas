unit stringUnit;

interface

uses
  TypeUnit{$ifdef gameclient}, objUnit{$endif};
const
  sPwdErr = '#R��̖�ܴa�e�`��Ոע���С����#n';
  sChineseDizhi: array[0..11] of string = (
    '��', '��', '��', 'î', '��', '��', '��', 'δ', '��', '��', '��', '��' );
  sChineseNumber: array[0..9] of string = (
    '��', 'һ', '��', '��', '��', '��', '��', '��', '��', '��');
  sGameTime = '%sʱ%s��';
  sUno = 'δ֪.';
  sNotLearned = '��δϰ�á�';
  sPlayer = '���';
  MenPaiNames: array[TGameMenPai] of string = (
    'δ��������', '���ƹٸ�', '������', '����ɽ', '��ħ��', '�칬', '����ɽ',
    '����', '��ׯ��', 'ʨ����', 'ħ��կ', '��ڤ�ظ�', '��˿��' );
  sBaseProps: array[0..7] of string = (
    'I D', '����', '�ȼ�', '����', '����', '��ν', '����', '����');
  sAttrs: array[0..5] of string = (
    '����', 'ħ��', '����', '����', '����', 'Ǳ��');
  sBltProps: array[0..5] of string = (
    '��Ѫ', 'ħ��', '����', '����', '�ٶ�', '����' );
  sPropLevJob = '%3d����%s';
  sJobNames: array[0..12] of string = (
    '', '������', '������', '��ң��', '������', '�����', '������', '��̫��',
    '��˪��', '������', '������', 'ʨħ��', '����΢' );
  sZhenFaName: array[0..MaxZhenfaID] of string = (
    '��ͨ�', '�츲�', '���d�', '�L�P�', '녴��', '���w�', '�����', '�B���', '����' );

  sPetProps: array[0..3] of string = (
    '���Y', '�y���ȼ�', '�ȼ�', '���\' );
  sPetZZs: array[TPetZz] of string = (
    '�����Y�|', '���R�Y�|', '�w���Y�|', '�����Y�|', '�ٶ��Y�|' );

  sNameAndLevel = '%s��(%d��)';

  sSlotEmpty = '��';

  sChatChns: array[0..15] of string = (
    '#cfffbf7[��ǰ]#n', '#cff6500[���]#n', '#c00fb00[˽��]#n', '#c00ffff[����]#n',
    '#ce7dff7[�؅^]#n', '#c0092ff[��ʾ]#n', '#ced00ed[����]#n', '#ccefbed[��]#n',
    '#ced0000[ϵ�y]#n', '#cdef7a5[�ȼ�]#n', '#ced0000[ϵ�y]#n', '#c84c3f7[����]#n',
    '#ced0000[ϵ�y]#n', '#cef6521[�M�]#n', '#cb5a2e7[��]#n', '#cf7be7b[����]#n' );
  ChatChnColors: array[0..15] of Cardinal = (
    $ffffffff, $ffff6500, $ffffffff, $ff00ffff, $ffffffff, $ffffff00, $ffff00ff,
    $ffffff00, $ffffffff, $ffce9aff, $ffffffff, $ff84c3ff, $ffffff00, $ffef6521 ,
    $ffb5a2e7 , $fff7be84 );
  sColon = '��';

  sMainMenuAttackTips: array[1..2] of string = ( '��ѡ����Ҫ#r����#n��Ŀ��', '��ѡ����Ҫ#y����#n��Ŀ��' );
  sMainMenuTradeTips: array[1..2] of string = ( '��ѡ����Ҫ#g����#n��Ŀ��', '��ѡ����Ҫ#y����#n��Ŀ��' );
  sMainMenuPartyTips = '��ѡ����Ҫ#g�������#n��Ŀ��';


  sBattleStart = '����ս��';
  sAttack = '�չ�';
  sReady = '����';
  sDodge = '����';
  sParry = '�м�';
  sCrit = '��';
  sDead = '����';
  sOut = '����';
  sFanjiLianjiZhuiji = '��������׷��';
  sChooseAtkTarget = '��ѡ����Ҫ�����ĵз�Ŀ��';
  sChooseEnemy = '��ѡ��һ���з�Ŀ��';
  sChooseAlias = '��ѡ��һ���ѷ�Ŀ��';
  sSkillTargetErr = '�޷��Ըö���ʹ�ô˼��ܣ�';
  sAttackTargetErr = '�޷������Ѿ�����';
  sNoSkillId = '����ѡ����Ҫʹ�õļ��ܣ�';
  sNoItemId = '����ѡ����Ҫʹ�õ���Ʒ��';
  sCaptureTargetErr = '�޷��Ըö���ʹ�ò�׽��';

  sInBattle = 'ս����ʹ��';
  sMedicine = '��ҩ#r��';

  sNoPlayerSelected = 'Ո���x��һ�����';
  sNoParty = '��û�ж��顭��';

  sItemPrice = '�r:%s';
  sEquipTypes: array[0..15] of string = (
    '�ӵ�', '����', 'ǹ�', '����', '����', '�̰�', '����', '˫��',
    '��ñ', '����', '��Ь',
    'Ůñ', 'Ů��', 'ŮЬ',
    '����', '����' );
  sEquipComposeTypes: array[0..2] of string = (
    '��ͨ����', 'ǿ������', 'ǿ������' );
  sLev = '%d��';
  sNameAmount = '%s��%d';
  sEqComponent1 = '[%d��%s֮�`] %d/1'#13'�]������Ž�������ُ�I';
  sEqComponent1Name = '%d��%s֮��';
  sEqComponent2: array[0..2] of string = (
    '%d��[%s]��[�b��ģ��] %d/1'#13'�]����������ұ�������ُ�I',
    '%d���u��[%s]'#13'%d���u��[%s]'#13'�]���F����ͨ�u��',
    '%d�������u��[%s] %d/1'#13'�]���F�������u��' );
  sEqComponent3 = '%d������[�����] %d/1'#13'�]��ұ������電�λُ�I';
  sEqComposeTili = '�����w����%d/%d';
  sItemKeyFmt = '��%s��';
  sItemLevel = '���ȼ���';
  sItemMaker = '�������ˡ�';
  sItemType = '�����͡�';
  sItemJob = '�����ý�ɫ��';
  sItemMakeLev: array[0..2] of string = ('', 'ϵͳ����', 'ϵͳǿ������');
  sItemNotFound = '�����e�]��������Ʒ��%s';
  sItemDeleteConfirm = '#RҪɾ��#Y[%s]#n��';

  sCostGold = '������X��%d/%d';
  sCostBindGold = '�������u��%d/%d';
  sCostJB = '������ţ�%d/%d';
  sCostSWB = '��������ţ�%d/%d';
  sRepairUseJB = '���M%d���Ŵ��������';
  sInsufficientGold = '�F���㣡';
  sInsufficientBindGold = '���u���㣡';

  sPartyCreateConfirm = '����ǰ�]����飬�Ƿ񄓽���';
  sPartyCreateSucc = '�������ɹ���';
  sPartyInvite = '����Ո��������飬��ͬ���N��';
  sPartyMemberName = '%s(%s)';
  {$IFDEF gameclient}
  sPartyStatus: array[TPartyMemberStatus] of string = (
    '����', '���x', '�x��' );
  {$ENDIF}

  sMallPrice = 'Ԫ����%d';

  sNoMarketItemSelected = 'Ո���x����Ҫُ�I����Ʒ';

  sPetChgNamePrompt = '�������µ����֣�';
  sDropPetErr1 = '����ǰ���Ƚ��������ָĻ�Ĭ�����֡�%s��';
  sDropPetErr2 = '��ս״̬�ĳ��ﲻ�ܷ���';
  sNoFocusPet = 'Ո���x��Ҫ�����Č��';
  sChooseLianhuaZZ = 'Ո�x��һ헸������Y�|����';
  sPetLianhuaResult = '����Ͷ�S�Y���飺#R%d#n��%s�����Y���飺#R%d#n��#Y%s�F�ڵ�%s�飺#R%d#n';
  sPetXibaobaoConfirm = '%s �ȼ����10����Ո�ȵ��L�����f�Fͨ̎�ύϴ����Ո��';

  sTradeLock: array[Boolean] of string = ('�c���i��', '�������i��');
  sTradeLock2: array[Boolean] of string = ('����δ�i��', '�������i��');
  sTradeSucc = '���׳ɹ���';
  sTradeUnlock = '�p���i��֮��ſ��Դ_�J���ף�';

  sExitGame = '#R�Ƿ�����˳�#Y��������2#n�Ĳ�����';
  sRelogin = '#Y�c�Α�������B���є��_���Ƿ����µ�䛣�';

  sSkillStudyExp = '���轛򞣺%d/%d';
  sSkillStudyBindGold = '�������u��%d/%d';

  sRWMB = '#Y�΄�Ŀ�ˣ�#n';
  sRWHF = '#Y�΄ջظ���#n';
  sRWGL = '#Y�΄չ��ԣ�#n';
  SRWMS = '#Y�΄�������#n';
  sQuestMapErr = '#R�΄�Ŀ�˲��ڮ�ǰ�؈D��#n';

  sTargetUnreachable = 'Ŀ�˲����Ƅӣ�';
  sAutoTalk = '�Ԅӌ�·��Մ��%s';

implementation

end.
