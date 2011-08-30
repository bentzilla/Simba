unit accountmanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, DOM, XMLWrite, XMLRead, DCPrijndael, DCPsha1;

type

  TStringArrayArray = Array of Array of String;

  { TAccountForm }

  TAccountForm = class(TForm)
    BOX_USER: TLabeledEdit;
    BOX_PASS: TLabeledEdit;
    BOX_PIN: TLabeledEdit;
    BUTTON_SAVE: TButton;
    BUTTON_ADD: TButton;
    BUTTON_DELETE: TButton;
    CHECKBOX_PIN: TCheckBox;
    CHECKBOX_PASS: TCheckBox;
    CHECKBOX_ACTIVE: TCheckBox;
    CHECKBOX_MEMBERS: TCheckBox;
    BOX_NICK: TLabeledEdit;
    BOX_ENCRYPT: TLabeledEdit;
    LIST_ACCOUNTS: TListBox;
    procedure BOX_PINKeyPress(Sender: TObject; var Key: char);
    procedure BOX_USERChange(Sender: TObject);
    procedure BUTTON_ADDClick(Sender: TObject);
    procedure BUTTON_DELETEClick(Sender: TObject);
    procedure BUTTON_SAVEClick(Sender: TObject);
    procedure CHECKBOX_PASSChange(Sender: TObject);
    procedure CHECKBOX_PINChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LIST_ACCOUNTSSelectionChange(Sender: TObject);
    function LIST_ACCOUNTSReturnSelectedIndex: Integer;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  AccountForm: TAccountForm;
  FileName: String;

implementation

{ TAccountForm }

procedure TAccountForm.FormCreate(Sender: TObject);
begin
  FileName := ExtractFileDir(Application.ExeName) + DirectorySeparator + 'accounts.xml';
end;

procedure TAccountForm.FormShow(Sender: TObject);
var
  Doc: TXMLDocument;
  i: integer;
  Exists: Boolean;
  s: String;
begin
  try
    if InputQuery('Salt', 'Enter Salt (Encryption Key of your choice)', s) then
      AccountForm.BOX_ENCRYPT.Text := s
    else
      AccountForm.BOX_ENCRYPT.Text := 'SALT';

    Exists := FileExists(FileName);

    // Load Document
    if (not (Exists)) then
      Exit
    else
      ReadXMLFile(Doc, FileName);

    // Load Accounts into List
    for i := 0 to (Doc.DocumentElement.ChildNodes.Count - 1) do
      LIST_ACCOUNTS.Items.Add(Doc.DocumentElement.ChildNodes.Item[i].FirstChild.TextContent);

  finally
    if (Exists) then
      Doc.Free;
  end;
end;

function DoEncrypt(Str, Salt: String): String;
var
  Cipher: TDCP_rijndael;
begin
  Cipher := TDCP_rijndael.Create(AccountForm);
  Cipher.InitStr(Salt, TDCP_sha1);
  Result := Cipher.EncryptString(Str);
  Cipher.Burn;
  Cipher.Free;
end;

function DoDecrypt(Str, Salt: String): String;
var
  Cipher: TDCP_rijndael;
begin
  Cipher := TDCP_rijndael.Create(AccountForm);
  Cipher.InitStr(Salt, TDCP_sha1);
  Result := Cipher.DecryptString(Str);
  Cipher.Burn;
  Cipher.Free;
end;

function TAccountForm.LIST_ACCOUNTSReturnSelectedIndex: Integer;
begin
  for Result := 0 to (LIST_ACCOUNTS.Count - 1) do
    if LIST_ACCOUNTS.Selected[Result] then
      Exit;
  Result := -1;
end;

procedure TAccountForm.BOX_PINKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in ['0'..'9', #8, #9]) then
    Key := #0;
end;

procedure TAccountForm.BOX_USERChange(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to (LIST_ACCOUNTS.Count - 1) do
    if LIST_ACCOUNTS.Selected[i] then
      LIST_ACCOUNTS.Items.Strings[i] := BOX_USER.Text;
end;

procedure TAccountForm.LIST_ACCOUNTSSelectionChange(Sender: TObject);
var
  Doc: TXMLDocument;
  Exists: Boolean;
begin
  try
    Exists := FileExists(FileName);

    // Load Document
    if (not (Exists)) then
      Exit
    else
      ReadXMLFile(Doc, FileName);

    // New Account
    if (Doc.DocumentElement.ChildNodes.Item[LIST_ACCOUNTSReturnSelectedIndex] = nil) then
    begin
      BOX_USER.Text := '';
      BOX_PASS.Text := '';
      BOX_NICK.Text := '';
      BOX_PIN.Text  := '';

      CHECKBOX_MEMBERS.Checked := False;
      CHECKBOX_ACTIVE.Checked  := False;

      Exit;
    end;

    // Load Account info into Boxes
    BOX_USER.Text := Doc.DocumentElement.ChildNodes.Item[LIST_ACCOUNTSReturnSelectedIndex].ChildNodes[0].TextContent;
    BOX_PASS.Text := DoDecrypt(Doc.DocumentElement.ChildNodes.Item[LIST_ACCOUNTSReturnSelectedIndex].ChildNodes[1].TextContent, AccountForm.BOX_ENCRYPT.Text);
    BOX_NICK.Text := Doc.DocumentElement.ChildNodes.Item[LIST_ACCOUNTSReturnSelectedIndex].ChildNodes[2].TextContent;
    BOX_PIN.Text  := DoDecrypt(Doc.DocumentElement.ChildNodes.Item[LIST_ACCOUNTSReturnSelectedIndex].ChildNodes[3].TextContent, AccountForm.BOX_ENCRYPT.Text);

    CHECKBOX_MEMBERS.Checked := StrToBool(Doc.DocumentElement.ChildNodes.Item[LIST_ACCOUNTSReturnSelectedIndex].ChildNodes[4].TextContent);
    CHECKBOX_ACTIVE.Checked  := StrToBool(Doc.DocumentElement.ChildNodes.Item[LIST_ACCOUNTSReturnSelectedIndex].ChildNodes[5].TextContent);

  finally
    if (Exists) then
      Doc.Free;

    BUTTON_DELETE.Enabled := True;
    BUTTON_SAVE.Enabled := True;
    BOX_USER.Enabled := True;
    BOX_PASS.Enabled := True;
    BOX_NICK.Enabled := True;
    BOX_PIN.Enabled := True;
    CHECKBOX_MEMBERS.Enabled := True;
    CHECKBOX_ACTIVE.Enabled := True;
  end;
end;

procedure TAccountForm.BUTTON_ADDClick(Sender: TObject);
begin
  LIST_ACCOUNTS.Items.Add('Account #' + IntToStr(LIST_ACCOUNTS.Count));
end;

procedure TAccountForm.BUTTON_DELETEClick(Sender: TObject);
var
  Doc: TXMLDocument;
  RootNode, ElementNode: TDOMNode;
  i, d: integer;
  Exists: Boolean;
begin
  try
    Exists := FileExists(FileName);
    d := LIST_ACCOUNTSReturnSelectedIndex;

    // Load Document
    if (not (Exists)) then
      Exit
    else
      ReadXMLFile(Doc, FileName);

    // Change id #'s

    // Root node
    RootNode := Doc.DocumentElement;

    for i := (d + 1) to (RootNode.ChildNodes.Count - 1) do
    begin
      ElementNode := RootNode.ChildNodes.Item[i];
      if ElementNode = nil then
        Break;
      TDOMElement(ElementNode).SetAttribute('id', IntToStr(i - 1));
    end;

    ElementNode := RootNode.ChildNodes.Item[d];
    if ElementNode = nil then
      Exit;
    RootNode.RemoveChild(ElementNode);
    WriteXMLFile(Doc, FileName);

  finally
    if (Exists) then
      Doc.Free;

    LIST_ACCOUNTS.Items.Delete(d);

    BUTTON_DELETE.Enabled := False;
    BUTTON_SAVE.Enabled := False;
    BOX_USER.Enabled := False;
    BOX_PASS.Enabled := False;
    BOX_NICK.Enabled := False;
    BOX_PIN.Enabled := False;
    CHECKBOX_MEMBERS.Enabled := False;
    CHECKBOX_ACTIVE.Enabled := False;
  end;
end;

procedure AddItem(iNode, tNode: TDOMNode; var eNode: TDOMNode);
begin
    iNode.AppendChild(tNode);
    eNode.AppendChild(iNode);
end;

procedure SaveToFile(const _FileName: String);
var
  Doc: TXMLDocument;
  RootNode, ElementNode, OldElementNode, ItemNode, TextNode: TDOMNode;
  i: integer;
  Exists: Boolean;
begin
  try
    Exists := FileExists(_FileName);
    i := AccountForm.LIST_ACCOUNTSReturnSelectedIndex;

    // Read/Create Document
    if (not (Exists)) then
    begin
      // Create a document
      Doc := TXMLDocument.Create;
      // Create a root node
      RootNode := Doc.CreateElement('Accounts');
      Doc.Appendchild(RootNode);
    end else
      ReadXMLFile(Doc, _FileName);

    // Root node
    RootNode := Doc.DocumentElement;

    // Nodes
    if Exists then
      OldElementNode := RootNode.ChildNodes.Item[i];

    ElementNode := Doc.CreateElement('Account');
    TDOMElement(ElementNode).SetAttribute('id', IntToStr(i));

    AddItem(Doc.CreateElement('Username'), Doc.CreateTextNode(AccountForm.BOX_USER.Text), ElementNode);
    AddItem(Doc.CreateElement('Password'), Doc.CreateTextNode(DoEncrypt(AccountForm.BOX_PASS.Text, AccountForm.BOX_ENCRYPT.Text)), ElementNode);
    AddItem(Doc.CreateElement('Nickname'), Doc.CreateTextNode(AccountForm.BOX_NICK.Text), ElementNode);
    AddItem(Doc.CreateElement('PIN'), Doc.CreateTextNode(DoEncrypt(AccountForm.BOX_PIN.Text, AccountForm.BOX_ENCRYPT.Text)), ElementNode);
    AddItem(Doc.CreateElement('Members'), Doc.CreateTextNode(BoolToStr(AccountForm.CHECKBOX_MEMBERS.Checked, 'True', 'False')), ElementNode);
    AddItem(Doc.CreateElement('Active'), Doc.CreateTextNode(BoolToStr(AccountForm.CHECKBOX_ACTIVE.Checked, 'True', 'False')), ElementNode);

    if ((OldElementNode = nil) or (not (Exists))) then
      RootNode.AppendChild(ElementNode)
    else
      RootNode.ReplaceChild(ElementNode, OldElementNode);

    // Save XML
    WriteXMLFile(Doc, _FileName);

  finally
    if (Exists) then
      Doc.Free;
  end;
end;

procedure TAccountForm.BUTTON_SAVEClick(Sender: TObject);
begin
  if not DirectoryExists(ExtractFileDir(Application.ExeName)) then
    CreateDir(ExtractFileDir(Application.ExeName));
  SaveToFile(FileName);
end;

procedure TAccountForm.CHECKBOX_PASSChange(Sender: TObject);
begin
  if CHECKBOX_PASS.Checked then
    BOX_PASS.EchoMode := emNormal
  else
    BOX_PASS.EchoMode := emPassword;
  AccountForm.Refresh;
end;

procedure TAccountForm.CHECKBOX_PINChange(Sender: TObject);
begin
  if CHECKBOX_PIN.Checked then
    BOX_PIN.EchoMode := emNormal
  else
    BOX_PIN.EchoMode := emPassword;
  AccountForm.Refresh;
end;

procedure TAccountForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

initialization
  {$R *.lfm}

end.
