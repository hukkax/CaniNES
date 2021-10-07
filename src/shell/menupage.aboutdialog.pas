unit MenuPage.AboutDialog;

{$mode delphi}

interface

uses
	Classes, SysUtils,
	Graphics32,
	NES.Types, MenuHandler;

type
	TAboutDialog = class(TMenuPageClass)
	public
		constructor Create(BrowserPage: TMenuPage); overload;
	end;


implementation

uses
	MainWindow, MainMenu,
	Basement.Renderer.Overlay,
	NES.Config;

// ============================================================================
// TCartInfoBrowser
// ============================================================================

constructor TAboutDialog.Create(BrowserPage: TMenuPage);

	function Add(const S: String): TMenuEntry; inline;
	begin
		Result := Page.AddText(S);
		if Result <> nil then
			Result.IsCentered := True;
	end;

begin
	inherited Create;

	Page := BrowserPage;
	Page.FullScreen := True;
	Page.Width := MenuRenderer.FrameBuffer.Width;
	Dec(Page.ItemHeight, 1);

	Page.Items.Clear;

	Page.HeaderCaption := 'About ' + APPNAME;
	Page.AddBackCommand;

	//----------------------------------------------------------

	Add(' ');
	Add(APPNAME + ' version ' + APPVERSION).Color := Palette[COLOR_MENU_NORMAL];
	Add(' ');
	Add('A NES emulator based on Mesen');
	Add(' ');

	Add(Format('Built with Free Pascal %s on %.4d-%.2d-%.2d', [{$I %FPCVERSION%},
		{$I %DATEYEAR%}, {$I %DATEMONTH%}, {$I %DATEDAY%}]));
	Add(Format('for target %s (%s)',
		[{$I %FPCTARGETCPU}, {$I %FPCTARGETOS}{$IFDEF DEBUG}+', debug build'{$ENDIF}]));
	Add(' ');

	Add('Mesen (C) M. Bibaud (Sour) and contributors');
	Add('URL: http://mesen.ca/');
	Add(' ');

	Add('nes_ntsc and blip_buffer (C) Shay Green (Blargg)');
	Add('URL: http://www.slack.net/~ant/');
	Add(' ');

	Add('FreePascal ports by Joel Toivonen (hukka)');
	Add('URL: https://github.com/hukkax/CaniNES');
end;



end.

