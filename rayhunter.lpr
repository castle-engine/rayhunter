{
  Copyright 2003-2017 Michalis Kamburelis.

  This file is part of "rayhunter".

  "rayhunter" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "rayhunter" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "rayhunter"; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA

  ----------------------------------------------------------------------------
}

program RayHunter;

uses SysUtils, CastleVectors, CastleRayTracer, CastleSceneCore,
  CastleImages, CastleUtils, CastleProgress, CastleProgressConsole,
  CastleParameters, CastleURIUtils,
  X3DFields, X3DNodes, CastleRays, CastleStringUtils, CastleApplicationProperties,
  CastleTimeUtils, Classes,
  { TODO: These are OpenGL-specific units, and we would prefer not to use
    them in rayhunter. Scene should be TCastleSceneCore (not TCastleScene),
    and scene manager should be... well, something not related to OpenGL.
    All this trouble is needed now to get BaseLights (containing headlight)
    from scene manager. }
  CastleSceneManager, CastleScene;

var
  { parametry podawane w linii polecen -------------------------------------- }
  { parametry ktore musza byc podane w linii polecen }
  RTDepth: Integer;
  RTKind: TRaytracerKind;
  PTNonPrimarySamplesCount: Cardinal; { meaningless if RTKind = rtkClassic }
  ImageWidth, ImageHeight: Cardinal;
  SceneURL, OutImageURL: string;

  { inne parametry ktore maja jakies defaultowe wartosci i nie musza byc zawsze
    podawane }
  { kolor tla, uzywany jesli BGImageURL = ''. }
  SceneBGColor: TVector3Single = (0, 0, 0);

  CamPos: TVector3Single;
  CamDir: TVector3Single;
  CamUp: TVector3Single;
  Param_CamPos: TVector3Single;
  Param_CamDir: TVector3Single;
  Param_CamUp: TVector3Single;
  WasParam_CamPos: boolean = false;
  WasParam_CamDir: boolean = false;
  WasParam_CamUp: boolean = false;

  { Note: Projection.PerspectiveAngles[1] = 0 means "unspecified",
    will be adjusted to image dims }
  Projection: TProjection;
  { Was Projection.ProjectionView value explicitly
    stated by command-line parameter. }
  ProjectionTypeExplicit: boolean = false;

  WritePartialRows: Cardinal = 0;
  WritePartialRows_LogFile: string = '';

  PTPrimarySamplesCount: Cardinal = 1;
  PTDirectIllumSamplesCount: Cardinal = 1;
  PTRRoulContinue: Single = 0.5;
  FirstRow: Cardinal = 0;

  { helper variables for doing the job --------------------------------------- }
  Scene: TCastleScene;
  Image: TCastleImage;

procedure PixelsMadeNotify(PixelsMadeCount: Cardinal; Data: Pointer);
begin
  if PixelsMadeCount mod ImageWidth <> 0 then Exit;

  Progress.Step;
  { jezeli WritePartialRows jest wlaczone i jezeli ilosc zrobionych wierszy
    jest podzielna przez WritePartialRows (i wieksza od zera) i jezeli
    nie zrobilismy jeszcze calego obrazka to zapisz obrazek czesciowy. }
  if (WritePartialRows > 0) and (PixelsMadeCount > 0) and
    ((PixelsMadeCount div ImageWidth) mod WritePartialRows = 0) and
     (PixelsMadeCount < ImageWidth * ImageHeight) then
  begin
    try
      SaveImage(Image, OutImageURL);
    except
      on E: Exception do
      begin
        Writeln(ErrOutput, Format(
          'Warning: Saving partial image to "%s" failed: %s',
          [OutImageURL, E.Message]));
        { In case of exception while SaveImage, write a warning
          (don't fail with error).
          Exit without writing to WritePartialRows_LogFile. }
        Exit;
      end;
    end;
    StringToFile(WritePartialRows_LogFile,
      IntToStr(PixelsMadeCount div ImageWidth));
  end;
end;

const
  Version = '1.3.4';
  Options: array [0..13] of TOption =
  ( (Short:  #0; Long: 'scene-bg-color'; Argument: oaRequired3Separate),
    (Short: 'p'; Long: 'camera-pos'; Argument: oaRequired3Separate),
    (Short: 'd'; Long: 'camera-dir'; Argument: oaRequired3Separate),
    (Short: 'u'; Long: 'camera-up'; Argument: oaRequired3Separate),
    (Short:  #0; Long: 'view-angle-x'; Argument: oaRequired),
    (Short:  #0; Long: 'force-view-angle-y'; Argument: oaRequired),
    (Short:  #0; Long: 'write-partial-rows'; Argument: oaRequired2Separate),
    (Short:  #0; Long: 'direct-illum-samples-count'; Argument: oaRequired),
    (Short:  #0; Long: 'r-roul-continue'; Argument: oaRequired),
    (Short:  #0; Long: 'primary-samples-count'; Argument: oaRequired),
    (Short:  #0; Long: 'first-row'; Argument: oaRequired),
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short: 'v'; Long: 'version'; Argument: oaNone),
    (Short: #0; Long: 'ortho'; Argument: oaRequired4Separate)
  );

  procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
    const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
  begin
    case OptionNum of
      0 : SceneBGColor := SeparateArgsToVector3Single(SeparateArgs);
      1 : begin Param_CamPos := SeparateArgsToVector3Single(SeparateArgs); WasParam_CamPos := true end;
      2 : begin Param_CamDir := SeparateArgsToVector3Single(SeparateArgs); WasParam_CamDir := true end;
      3 : begin Param_CamUp := SeparateArgsToVector3Single(SeparateArgs); WasParam_CamUp := true end;
      4 : begin ProjectionTypeExplicit := true; Projection.ProjectionType := ptPerspective; Projection.PerspectiveAngles[0] := StrToFloat(Argument); end;
      5 : begin ProjectionTypeExplicit := true; Projection.ProjectionType := ptPerspective; Projection.PerspectiveAngles[1] := StrToFloat(Argument); end;
      6 : begin
            WritePartialRows := StrToInt(SeparateArgs[1]);
            WritePartialRows_LogFile := SeparateArgs[2];
          end;
      7 : PTDirectIllumSamplesCount := StrToInt(Argument);
      8 : PTRRoulContinue := StrToFloat(Argument);
      9 : PTPrimarySamplesCount := StrToInt(Argument);
      10: FirstRow := StrToInt(Argument);
      11: begin
            InfoWrite(
             {'0123456789012345678901234567890123456789012345678901234567890123456789012345' }
              'RayHunter: ray-tracer for VRML / X3D (and others, like  3DS) models.' +nl+
              'Classic (Whitted-style) ray-tracer (based on local lighting equations' +nl+
              'from VRML 97 / X3D specification) and simple Monte Carlo path tracer are' +nl+
              'available.' +nl+
              nl+
              'Usage:' +nl+
              '  rayhunter [OPTIONS]... classic DEPTH IMAGE-SIZE-X IMAGE-SIZE-Y' +nl+
              '    SCENE-URL OUT-IMAGE-URL' +nl+
              'for classic raytracer or' +nl+
              '  rayhunter [OPTIONS]... path DEPTH NON-PRIMARY-SAMPLES-COUNT' +nl+
              '    IMAGE-SIZE-X IMAGE-SIZE-Y SCENE-URL OUT-IMAGE-URL' +nl+
              'for path tracer.' +nl+
              nl+
              'Options can be actually anywhere on the command-line, mixed between' +nl+
              'the required parameters shown in the above "usage" specification.' +nl+
              'Accepted options are:' +nl+
              HelpOptionHelp +nl+
              VersionOptionHelp +nl+
              '  -p / --camera-pos POS.X POS.Y POS.Z ,' +nl+
              '  -d / --camera-dir DIR.X DIR.Y DIR.Z ,' +nl+
              '  -u / --camera-up  UP.X  UP.Y  UP.Z' +nl+
              '                        Set initial camera position, direction and up' +nl+
              '  --view-angle-x ANGLE  Set horizontal viewing angle (in degrees).' +nl+
              '  --force-view-angle-y ANGLE' +nl+
              '                        Set vertical viewing angle (in degrees).' +nl+
              '  --ortho LEFT BOTTOM RIGHT TOP' +nl+
              '                        Use orthographic projection with given dimensions.' +nl+
              '  --scene-bg-color RED GREEN BLUE' +nl+
              '                        Set color emitted by scene background' +nl+
              '  --write-partial-rows ROWS LOG-ROWS-FILE' +nl+
              '                        Causes partial result to be saved to the' +nl+
              '                        OUT-IMAGE-URL after generating each ROWS rows.' +nl+
              '                        Additionally, in LOG-ROWS-FILE file number' +nl+
              '                        of written rows will be recorded.' +nl+
              '                        Value 0 for ROWS (default) prevents from writing' +nl+
              '                        partial result' +nl+
              '  --first-row ROWS      Assume ROWS rows were already generated and saved' +nl+
              '                        in OUT-IMAGE-URL' +nl+
              nl+
              'Options meaningfull only for path tracer (ignored if supplied for classic' +nl+
              'raytracer):' +nl+
              '  --direct-illum-samples-count COUNT' +nl+
              '                        Set direct illumination samples count (default 1)' +nl+
              '  --r-roul-continue ALPHA' +nl+
              '                        Set Roussian Roulette parameter for continuation' +nl+
              '                        (e.g. 0.75 gives longer paths than 0.5)' +nl+
              '  --primary-samples-count COUNT' +nl+
              '                        Set primary samples count (default 1)' +nl+
              nl+
              SCastleEngineProgramHelpSuffix('rayhunter', Version, true));
            Halt;
          end;
      12: begin
            Writeln(Version);
            Halt;
          end;
      13 :begin
            Projection.ProjectionType := ptOrthographic;
            ProjectionTypeExplicit := true;
            Projection.OrthoDimensions[0] := StrToFloat(SeparateArgs[1]);
            Projection.OrthoDimensions[1] := StrToFloat(SeparateArgs[2]);
            Projection.OrthoDimensions[2] := StrToFloat(SeparateArgs[3]);
            Projection.OrthoDimensions[3] := StrToFloat(SeparateArgs[4]);
          end;
      else raise EInternalError.Create('OptionProc');
    end;
  end;

var
  { rest of helper variables }
  OutImageClass: TCastleImageClass;
  MyRayTracer: TRayTracer;
  DummyGravityUp: TVector3Single;
  ModelProjectionType: TProjectionType;
  Viewpoint: TAbstractViewpointNode;
  FieldOfView: TMFFloat;
  SceneManager: TCastleSceneManager;
  Stats: TStringList;
begin
  { defaults for Projection }
  Projection.ProjectionType := ptPerspective;
  Projection.PerspectiveAngles := Vector2Single(60, 0);
  Projection.OrthoDimensions := Vector4Single(-1, -1, 1, 1);

  { parsing parameters with no assigned positions }
  Parameters.Parse(Options, @OptionProc, nil);
  { parsing parameters with assigned positions }
  Parameters.CheckHighAtLeast(6);
  case ArrayPosText(Parameters[1], ['classic', 'path']) of
    0: begin RTKind := rtkClassic;    Parameters.CheckHigh(6) end;
    1: begin RTKind := rtkPathTracer; Parameters.CheckHigh(7) end;
    else raise EInvalidParams.Create('Invalid RayTracer kind : expected "classic" or "path", got ' + Parameters[1]);
  end;
  Parameters.Delete(0);
  RTDepth := StrToInt(Parameters[1]); Parameters.Delete(0);
  if RTKind = rtkPathTracer then
  begin
    PTNonPrimarySamplesCount := StrToInt(Parameters[1]);
    Parameters.Delete(0);
  end;
  ImageWidth := StrToInt(Parameters[1]); Parameters.Delete(0);
  ImageHeight := StrToInt(Parameters[1]); Parameters.Delete(0);
  SceneURL := Parameters[1]; Parameters.Delete(0);
  OutImageURL := Parameters[1]; Parameters.Delete(0);

  { register progres showing on console }
  Progress.UserInterface := ProgressConsoleInterface;

  { init some vars to nil values (to allow simple try..finally..end clause
    instead of nested try..try.. ... finally .. finally ...end) }
  Scene := nil;
  Image := nil;
  SceneManager := nil;
  Stats := nil;
  MyRayTracer := nil;

  try
    { read scene and build SceneOctree }
    ApplicationProperties.OnWarning.Add(@ApplicationProperties.WriteWarningOnConsole);
    Write('Reading scene from file "'+URICaption(sceneURL)+'"... ');
    Scene := TCastleScene.Create(nil);
    Scene.Load(SceneURL, true);
    Writeln('done.');
    Writeln(Format('Scene contains %d triangles and %d vertices.',
      [Scene.TrianglesCount(false),
       Scene.VerticesCount(false)]));

    { calculate Scene.TriangleOctree }
    Scene.TriangleOctreeProgressTitle := 'Building octree';
    Scene.Spatial := [ssVisibleTriangles];

    { calculate SceneManager (will be used for headlight in BaseLights) }
    SceneManager := TCastleSceneManager.Create(nil);
    SceneManager.MainScene := Scene;
    SceneManager.Items.Add(Scene);

    { calculate CamPos/Dir/Up }
    Viewpoint := Scene.GetViewpoint(ModelProjectionType, CamPos, CamDir, CamUp, DummyGravityUp);

    if not ProjectionTypeExplicit then
    begin
      { If user didn't choose explicitly perspective or orthographic projection,
        then viewpoint node determines it. }
      Projection.ProjectionType := ModelProjectionType;
      if (Viewpoint <> nil) and
         (Viewpoint is TOrthoViewpointNode) then
      begin
        { So we know that user didn't also explicitly specify ortho dimensions.
          So use the ones from viewpoint. }
        FieldOfView := TOrthoViewpointNode(Viewpoint).FdFieldOfView;
        if FieldOfView.Count > 0 then Projection.OrthoDimensions[0] := FieldOfView.Items[0];
        if FieldOfView.Count > 1 then Projection.OrthoDimensions[1] := FieldOfView.Items[1];
        if FieldOfView.Count > 2 then Projection.OrthoDimensions[2] := FieldOfView.Items[2];
        if FieldOfView.Count > 3 then Projection.OrthoDimensions[3] := FieldOfView.Items[3];
      end else
      if (Viewpoint <> nil) and
         (Viewpoint is TOrthographicCameraNode_1) then
      begin
        Projection.OrthoDimensions[0] := -TOrthographicCameraNode_1(Viewpoint).FdHeight.Value / 2;
        Projection.OrthoDimensions[1] := -TOrthographicCameraNode_1(Viewpoint).FdHeight.Value / 2;
        Projection.OrthoDimensions[2] :=  TOrthographicCameraNode_1(Viewpoint).FdHeight.Value / 2;
        Projection.OrthoDimensions[3] :=  TOrthographicCameraNode_1(Viewpoint).FdHeight.Value / 2;
      end;
    end;

    if WasParam_CamPos then CamPos := Param_CamPos;
    if WasParam_CamDir then CamDir := Param_CamDir;
    if WasParam_CamUp  then CamUp := Param_CamUp ;

    { calculate Image }
    { calculate OutImageKind }
    OutImageClass := ImageClassBestForSavingToFormat(OutImageURL);
    { try load image from OutImageURL if FirstRow > 0 }
    if FirstRow > 0 then
    try
      Image := LoadImage(OutImageURL, [OutImageClass], ImageWidth, ImageHeight);
    except {silence any exception} end;
    { if not FirstRow = 0 or loading from OutImageURL failed : init clear image }
    if Image = nil then
    begin
      Image := OutImageClass.Create(ImageWidth, ImageHeight);
      { init image. clear image to SceneBGColor - raytracer zapisze caly obrazek
        i zakryje to tlo ale ciagle gdy bedziemy zapisywac write-partial-rows to
        to tlo bedzie widoczne. A nie chcemy zeby byly tam widoczne jakies smieci
        typowe dla niezainicjowanej pamieci. }
      if Image is TRGBFloatImage then
        TRGBFloatImage(Image).Clear(SceneBGColor) else
      if Image is TRGBImage then
        Image.Clear(Vector4Byte(Vector4Single(SceneBGColor, 0))) else
        raise EInternalError.Create('inv OutImageClass');
    end;

    { init ViewAngleY }
    if Projection.PerspectiveAngles[1] = 0.0 then
      Projection.PerspectiveAngles[1] := AdjustViewAngleDegToAspectRatio(
        Projection.PerspectiveAngles[0], ImageHeight/ImageWidth);

    { create MyRayTracer instance, set it's properties }
    case RTKind of
      rtkClassic:
        begin
          MyRayTracer := TClassicRayTracer.Create;
          TClassicRayTracer(MyRayTracer).InitialDepth := RTDepth;
          TClassicRayTracer(MyRayTracer).FogNode := Scene.FogStack.Top;
          TClassicRayTracer(MyRayTracer).BaseLights := SceneManager.BaseLights;
        end;
      rtkPathTracer:
        begin
          MyRayTracer := TPathTracer.Create;
          TPathTracer(MyRayTracer).MinDepth := RTDepth;
          TPathTracer(MyRayTracer).RRoulContinue := PTRRoulContinue;
          TPathTracer(MyRayTracer).PrimarySamplesCount := PTPrimarySamplesCount;
          TPathTracer(MyRayTracer).NonPrimarySamplesCount := PTNonPrimarySamplesCount;
          TPathTracer(MyRayTracer).DirectIllumSamplesCount := PTDirectIllumSamplesCount;
        end;
    end;
    MyRayTracer.Image := Image;
    MyRayTracer.Octree := Scene.InternalOctreeVisibleTriangles;
    MyRayTracer.CamPosition := CamPos;
    MyRayTracer.CamDirection := CamDir;
    MyRayTracer.CamUp := CamUp;
    MyRayTracer.Projection := Projection;
    MyRayTracer.SceneBGColor := SceneBGColor;
    MyRayTracer.PixelsMadeNotifier := @PixelsMadeNotify;
    MyRayTracer.FirstPixel :=  FirstRow * Image.Width;

    { go ! }
    Stats := TStringList.Create;
    Progress.Init(ImageHeight-FirstRow, 'Rendering');
    try
      MyRayTracer.ExecuteStats(Stats);
    finally Progress.Fini; end;

    Writeln(Stats.Text);

    SaveImage(Image, OutImageURL);
  finally
    FreeAndNil(Scene);
    FreeAndNil(Image);
    FreeAndNil(SceneManager);
    FreeAndNil(Stats);
    FreeAndNil(MyRayTracer);
  end;
end.
