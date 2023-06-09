--  SPDX-FileCopyrightText: 2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with AWS.Client;
with AWS.Headers;
with AWS.Messages;
with AWS.Parameters;
with AWS.Response;

with League.Base_Codecs;
with League.Holders;
with League.JSON.Arrays;
with League.JSON.Documents;
with League.JSON.Objects;
with League.JSON.Values;
with League.Settings;
with League.Stream_Element_Vectors;
with League.String_Vectors;

with Servlet.Contexts;
with Servlet.HTTP_Sessions;
with XML.SAX.Input_Sources.Streams.Files;
with XML.SAX.Output_Destinations.Strings;
with XML.SAX.Pretty_Writers;
with XML.SAX.Simple_Readers;
with XML.Templates.Processors;

with Ada.Text_IO;

package body Servlet.OAuth is

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
        renames League.Strings.To_Universal_String;

   procedure Get_Token
     (Self  : OAuth_Provider;
      Code  : League.Strings.Universal_String;
      Token : out League.Strings.Universal_String;
      EMail : out League.Strings.Universal_String);

   procedure Decode_Facebook_Token
     (Token : League.Strings.Universal_String;
      Info  : out Mimsy.Sessions.User_Info);

   procedure Decode_Github_Token
     (Token : League.Strings.Universal_String;
      Info  : out Mimsy.Sessions.User_Info);

   procedure Decode_Google_Token
     (Token : League.Strings.Universal_String;
      Info  : out Mimsy.Sessions.User_Info);

   procedure Trace (Text : String);

   Use_Trace : constant Boolean := True;

   protected body State_Cache is

      ---------------
      -- Check_Key --
      ---------------

      function Check_Key
        (Session_Id : League.Strings.Universal_String;
         Key        : League.Strings.Universal_String)
         return Boolean
      is
         use type League.Strings.Universal_String;
      begin
         return Map.Contains (Session_Id) and then
           Map (Session_Id).State = Key;
      end Check_Key;

      ----------------
      -- Create_Key --
      ----------------

      procedure Create_Key
        (Session_Id  : League.Strings.Universal_String;
         Return_Path : League.Strings.Universal_String;
         Result      : out League.Strings.Universal_String)
      is
         use type Ada.Containers.Count_Type;
         Max_States : constant := 100;
         Cursor : String_Lists.Cursor;
         Data   : League.Stream_Element_Vectors.Stream_Element_Vector;
      begin
         if Map.Contains (Session_Id) then
            Cursor := Queue.Find (Session_Id);
            Queue.Delete (Cursor);
            Queue.Append (Session_Id);
            Result := Map (Session_Id).State;
            --  Well, GNAT complains if we just update Path component
            Map (Session_Id) := (Result, Return_Path);
            return;
         end if;

         for J in 1 .. 12 loop
            Data.Append (Stream_Element_Random.Random (Random));
         end loop;

         Result := League.Base_Codecs.To_Base_64_URL (Data);

         Map.Insert (Session_Id, (Result, Return_Path));
         Queue.Append (Session_Id);

         if Map.Length > Max_States then
            Map.Delete (Queue.First_Element);
            Queue.Delete_First;
         end if;
      end Create_Key;

   ---------------------
   -- Get_Return_Path --
   ---------------------

      function Get_Return_Path
        (Session_Id : League.Strings.Universal_String)
         return League.Strings.Universal_String is
      begin
         return Map (Session_Id).Path;
      end Get_Return_Path;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize is
      begin
         Stream_Element_Random.Reset (Random);
      end Initialize;

   end State_Cache;

   ---------------------------
   -- Decode_Facebook_Token --
   ---------------------------

   procedure Decode_Facebook_Token
     (Token : League.Strings.Universal_String;
      Info  : out Mimsy.Sessions.User_Info)
   is
      Parameters : AWS.Parameters.List;
      Document   : League.JSON.Documents.JSON_Document;
      Object     : League.JSON.Objects.JSON_Object;
   begin
      Parameters.Add ("access_token", Token.To_UTF_8_String);
      Parameters.Add ("fields", "id,name,email,picture");

      declare
         Full : constant String := AWS.Parameters.URI_Format (Parameters);
         Data : AWS.Response.Data;
      begin
         Data := AWS.Client.Post
           (URL          => "https://graph.facebook.com/v2.11/me",
            Data         => Full (Full'First + 1 .. Full'Last),
            Content_Type => "application/x-www-form-urlencoded");

         Trace ("Decode_Facebook_Token: " & AWS.Messages.Status_Code'Image
                (AWS.Response.Status_Code (Data)));
         Trace (AWS.Response.Message_Body (Data));

         if AWS.Response.Status_Code (Data) in AWS.Messages.S200 then
            Document := League.JSON.Documents.From_JSON
              (AWS.Response.Message_Body (Data));
            Object := Document.To_JSON_Object;
            Info.User := Object.Value (+"id").To_String;
            Info.Name := Object.Value (+"name").To_String;
            Info.Mails.Append (Object.Value (+"email").To_String);
            Object := Object.Value (+"picture").To_Object;
            Object := Object.Value (+"data").To_Object;
            Info.Avatar := Object.Value (+"url").To_String;
         end if;
      end;
   end Decode_Facebook_Token;

   -------------------------
   -- Decode_Github_Token --
   -------------------------

   procedure Decode_Github_Token
     (Token : League.Strings.Universal_String;
      Info  : out Mimsy.Sessions.User_Info)
   is
      Headers    : AWS.Headers.List;
      Data       : AWS.Response.Data;
      Mail       : League.Strings.Universal_String;
      Document   : League.JSON.Documents.JSON_Document;
      Object     : League.JSON.Objects.JSON_Object;
      Vector     : League.JSON.Arrays.JSON_Array;
   begin
      Headers.Add ("Authorization", "token " & Token.To_UTF_8_String);
      Headers.Add ("Accept", "application/vnd.github.v3+json");

      Data := AWS.Client.Get
        (URL     => "https://api.github.com/user",
         Headers => Headers);

      Trace ("Decode_Github_Token: " & AWS.Messages.Status_Code'Image
             (AWS.Response.Status_Code (Data)));
      Trace (AWS.Response.Message_Body (Data));

      if AWS.Response.Status_Code (Data) in AWS.Messages.S200 then
         Document := League.JSON.Documents.From_JSON
           (AWS.Response.Message_Body (Data));
         Object := Document.To_JSON_Object;
         Info.User := Object.Value (+"login").To_String;  --  id???
         Info.Name := Object.Value (+"name").To_String;
         Info.Avatar := Object.Value (+"avatar_url").To_String;
         Mail := Object.Value (+"email").To_String;

         Data := AWS.Client.Get
           (URL     => "https://api.github.com/user/emails",
            Headers => Headers);

         if AWS.Response.Status_Code (Data) in AWS.Messages.S200 then
            Document := League.JSON.Documents.From_JSON
              (AWS.Response.Message_Body (Data));
            Vector := Document.To_JSON_Array;

            for J in 1 .. Vector.Length loop
               Object := Vector.Element (J).To_Object;

               if Object.Value (+"verified").To_Boolean then
                  if Object.Value (+"primary").To_Boolean then
                     Info.Mails.Insert (1, Object.Value (+"email").To_String);
                  else
                     Info.Mails.Append (Object.Value (+"email").To_String);
                  end if;
               end if;
            end loop;
         end if;

         if Info.Mails.Index (Mail) = 0 and not Mail.Is_Empty then
            Info.Mails.Append (Mail);
         end if;
      end if;
   end Decode_Github_Token;

   -------------------------
   -- Decode_Google_Token --
   -------------------------

   procedure Decode_Google_Token
     (Token : League.Strings.Universal_String;
      Info  : out Mimsy.Sessions.User_Info)
   is
      Parts      : League.String_Vectors.Universal_String_Vector;
      Encoded    : League.Strings.Universal_String;
      Decoded    : League.Stream_Element_Vectors.Stream_Element_Vector;
      Claim_Set  : League.JSON.Documents.JSON_Document;
      Object     : League.JSON.Objects.JSON_Object;
   begin
      Parts := Token.Split ('.');
      Encoded := Parts.Element (2);

      while Encoded.Length mod 4 /= 0 loop
         Encoded.Append ('=');
      end loop;

      Decoded := League.Base_Codecs.From_Base_64_URL (Encoded);

      declare
         Raw : Ada.Streams.Stream_Element_Array :=
           Decoded.To_Stream_Element_Array;
         Text : String (1 .. Raw'Length);
         pragma Import (Ada, Text);
         for Text'Address use Raw'Address;
      begin
         Trace ("Decode_Google_Token:");
         Trace (Text);
      end;

      Claim_Set := League.JSON.Documents.From_JSON (Decoded);
      Object := Claim_Set.To_JSON_Object;
      Info.User := Object.Value (+"sub").To_String;
      Info.Mails.Append (Object.Value (+"email").To_String);
      Info.Name := Object.Value (+"name").To_String;
      Info.Avatar := Object.Value (+"picture").To_String;
   end Decode_Google_Token;

   ------------
   -- Do_Get --
   ------------

   overriding procedure Do_Get
     (Self     : in out OAuth_Servlet;
      Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class)
   is
      function Login_Page return League.Strings.Universal_String;

      Context   : constant access Servlet.Contexts.Servlet_Context'Class
        := Request.Get_Servlet_Context;

      Session : constant League.Strings.Universal_String :=
        Request.Get_Session.Get_Id;

      Path    : constant League.Strings.Universal_String :=
        Request.Get_Path_Info;

      function Login_Page return League.Strings.Universal_String is
         Key       : League.Strings.Universal_String;
         Path      : constant League.Strings.Universal_String :=
           Request.Get_Parameter (+"path");
         XHTML     : constant League.Strings.Universal_String :=
           +"/login.xhtml.tmpl";
         Input     : aliased XML.SAX.Input_Sources.Streams.Files
           .File_Input_Source;
         Reader    : aliased XML.SAX.Simple_Readers.Simple_Reader;
         Filter    : aliased XML.Templates.Processors.Template_Processor;
         Writer    : aliased XML.SAX.Pretty_Writers.XML_Pretty_Writer;
         Output    : aliased XML.SAX.Output_Destinations.Strings
           .String_Output_Destination;
      begin
         Self.Cache.Create_Key (Session, Path, Key);
         --  Set template input
         Input.Open_By_File_Name (Context.Get_Real_Path (XHTML));

         --  Configure reader
         Reader.Set_Input_Source (Input'Unchecked_Access);
         Reader.Set_Content_Handler (Filter'Unchecked_Access);
         Reader.Set_Lexical_Handler (Filter'Unchecked_Access);

         --  Configure template processor
         Filter.Set_Content_Handler (Writer'Unchecked_Access);
         Filter.Set_Lexical_Handler (Writer'Unchecked_Access);

         --  Bind wiki page content and path (origin URI)
         Filter.Set_Parameter (+"state", League.Holders.To_Holder (Key));

         for P in Self.OAuth_Providers.Iterate loop
            Filter.Set_Parameter
              (OAuth_Provider_Maps.Key (P),
               League.Holders.To_Holder (Self.OAuth_Providers (P).Client_Id));
         end loop;

         --  Configure XML writer.

         Writer.Set_Output_Destination (Output'Unchecked_Access);
         --  Process template
         Reader.Parse;

         return Output.Get_Text;
      end Login_Page;

      use type League.Strings.Universal_String;

      Token : League.Strings.Universal_String;
      EMail : League.Strings.Universal_String;
      Info  : Mimsy.Sessions.User_Info;
   begin
      if Self.OAuth_Providers.Contains (Path.Tail_From (2)) then
         if Self.Cache.Check_Key
           (Session, Request.Get_Parameter (+"state"))
         then
            Get_Token
              (Self.OAuth_Providers (Path.Tail_From (2)),
               Request.Get_Parameter (+"code"),
               Token, EMail);

            if Path = +"/google" then
               Decode_Google_Token (Token, Info);
            elsif Path = +"/facebook" then
               Decode_Facebook_Token (Token, Info);
            elsif Path = +"/github" then
               Decode_Github_Token (Token, Info);
            elsif Path = +"/mimsy" then
               Info.User := +"user-1";
               Info.Name := +"User";
               EMail := +"user@localhost";
            end if;

            if not EMail.Is_Empty then
               Info.Mails.Append (EMail);
            end if;

            if Info.Mails.Length > 0 then
               EMail := Info.Mails (1);

               if Info.User.Is_Empty then
                  Info.User := EMail;
               end if;

               if Info.Name.Is_Empty then
                  Info.Name := Info.User;
               end if;
            end if;

            Self.Handler.Do_Login
              (Info, Self.Cache.Get_Return_Path (Session), Request, Response);

            return;
         else
            Response.Set_Status (Servlet.HTTP_Responses.Unauthorized);
            return;
         end if;
      end if;

      Response.Set_Status (Servlet.HTTP_Responses.OK);
      Response.Set_Header (+"Cache-Control", +"no-cache");
      Response.Set_Content_Type (+"text/html");
      Response.Set_Character_Encoding (+"utf-8");

      Response.Get_Output_Stream.Write (Login_Page);
   end Do_Get;

   ----------------------
   -- Get_Servlet_Info --
   ----------------------

   overriding function Get_Servlet_Info
     (Self : OAuth_Servlet)
      return League.Strings.Universal_String
   is
      pragma Unreferenced (Self);
   begin
      return +"OAuth Servlet";
   end Get_Servlet_Info;

   ---------------
   -- Get_Token --
   ---------------

   procedure Get_Token
     (Self  : OAuth_Provider;
      Code  : League.Strings.Universal_String;
      Token : out League.Strings.Universal_String;
      EMail : out League.Strings.Universal_String)
   is
      Headers    : AWS.Headers.List;
      Parameters : AWS.Parameters.List;
      Document   : League.JSON.Documents.JSON_Document;
      Object     : League.JSON.Objects.JSON_Object;
   begin
      Headers.Add ("Accept", "application/json");
      Parameters.Add ("code", Code.To_UTF_8_String);
      Parameters.Add ("client_id", Self.Client_Id.To_UTF_8_String);
      Parameters.Add ("client_secret", Self.Client_Secret.To_UTF_8_String);
      Parameters.Add ("redirect_uri", Self.Redirect_URI.To_UTF_8_String);
      Parameters.Add ("grant_type", "authorization_code");

      declare
         Full : constant String := AWS.Parameters.URI_Format (Parameters);
         Data : AWS.Response.Data;
      begin
         Data := AWS.Client.Post
           (URL          => Self.Token_End_Point.To_UTF_8_String,
            Data         => Full (Full'First + 1 .. Full'Last),
            Content_Type => "application/x-www-form-urlencoded",
            Headers      => Headers);

         Trace ("Get_Token: " & AWS.Messages.Status_Code'Image
                (AWS.Response.Status_Code (Data)));
         Trace (AWS.Response.Message_Body (Data));

         if AWS.Response.Status_Code (Data) in AWS.Messages.S200 then
            Document := League.JSON.Documents.From_JSON
              (AWS.Response.Message_Body (Data));
            Object := Document.To_JSON_Object;
            Token := Object.Value (Self.Token_Key).To_String;

            if Object.Contains (+"email") then
               EMail := Object.Value (+"email").To_String;
            end if;
         end if;
      end;
   end Get_Token;

   -----------------
   -- Instantiate --
   -----------------

   overriding function Instantiate
     (Parameters : not null access
       Servlet.Generic_Servlets.Instantiation_Parameters'Class)
      return OAuth_Servlet
   is
      pragma Unreferenced (Parameters);

      procedure Add_OAuth_Provider
        (Map      : in out OAuth_Provider_Maps.Map;
         Name     : League.Strings.Universal_String;
         Settings : League.Settings.Settings);

      ------------------------
      -- Add_OAuth_Provider --
      ------------------------

      procedure Add_OAuth_Provider
        (Map      : in out OAuth_Provider_Maps.Map;
         Name     : League.Strings.Universal_String;
         Settings : League.Settings.Settings)
      is
         use type League.Strings.Universal_String;

         Prefix    : constant League.Strings.Universal_String :=
           "/" & Name & "/";
         Client_Id : constant League.Holders.Holder :=
           Settings.Value (Prefix & "client_id");
         Token_End_Point : constant League.Holders.Holder :=
           Settings.Value (Prefix & "token_endpoint");
         Client_Secret : constant League.Holders.Holder :=
           Settings.Value (Prefix & "client_secret");
         Redirect_URI : constant League.Holders.Holder :=
           Settings.Value (Prefix & "redirect_uri");
         Token_Key : constant League.Holders.Holder :=
           Settings.Value (Prefix & "token_key");
         Secure_Key : constant League.Holders.Holder :=
           Settings.Value (Prefix & "secure_key");
         Item : OAuth_Provider;
      begin
         Item.Client_Id := League.Holders.Element (Client_Id);
         Item.Token_End_Point := League.Holders.Element (Token_End_Point);
         Item.Client_Secret := League.Holders.Element (Client_Secret);
         Item.Redirect_URI := League.Holders.Element (Redirect_URI);
         Item.Token_Key := League.Holders.Element (Token_Key);
         if not League.Holders.Is_Empty (Secure_Key) then
            Item.Secure_Key := League.Holders.Element (Secure_Key);
         end if;

         Map.Insert (Name, Item);
      end Add_OAuth_Provider;

      Settings  : League.Settings.Settings;
   begin
      return Result : OAuth_Servlet do
         Result.Cache.Initialize;
         Add_OAuth_Provider (Result.OAuth_Providers, +"facebook", Settings);
         Add_OAuth_Provider (Result.OAuth_Providers, +"github", Settings);
         Add_OAuth_Provider (Result.OAuth_Providers, +"google", Settings);
         Add_OAuth_Provider (Result.OAuth_Providers, +"mimsy", Settings);
      end return;
   end Instantiate;

   -----------------
   -- Set_Handler --
   -----------------

   not overriding procedure Set_Handler
    (Self  : in out OAuth_Servlet;
     Value : access Login_Handler'Class) is
   begin
      Self.Handler := Value;
   end Set_Handler;

   -----------
   -- Trace --
   -----------

   procedure Trace (Text : String) is
   begin
      if Use_Trace then
         Ada.Text_IO.Put_Line (Text);
      end if;
   end Trace;

end Servlet.OAuth;
