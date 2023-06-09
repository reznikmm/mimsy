--  SPDX-FileCopyrightText: 2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Calendar.Formatting;

with XML.SAX.Input_Sources.Streams.Files;
with XML.SAX.Output_Destinations.Strings;
with XML.SAX.Pretty_Writers;
with XML.SAX.Simple_Readers;
with XML.Templates.Processors;

with League.Settings;
with League.Base_Codecs;
with League.Holders;
with League.IRIs;
with League.JSON.Documents;
with League.JSON.Objects;
with League.JSON.Values;
with League.Stream_Element_Vectors;
with League.String_Vectors;

with Servlet.Contexts;

with JWS;

package body Servlet.OAuth2_Server is

   use type League.Strings.Universal_String;

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
        renames League.Strings.To_Universal_String;

   function Generate_Code (Self : OAuth2_Server_Servlet'Class)
     return Authorization_Code;

   function Login_Page
     (Self    : OAuth2_Server_Servlet'Class;
      Request : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class)
      return League.Strings.Universal_String;

   procedure Do_Post_Sign_In
     (Self     : in out OAuth2_Server_Servlet'Class;
      Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class);

   procedure Do_Post_Token_Exchange
     (Self     : in out OAuth2_Server_Servlet'Class;
      Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class);

   function Check_User_Password
     (Self     : OAuth2_Server_Servlet'Class;
      User     : League.Strings.Universal_String;
      Password : League.Strings.Universal_String) return Boolean;

   -------------------------
   -- Check_User_Password --
   -------------------------

   function Check_User_Password
     (Self     : OAuth2_Server_Servlet'Class;
      User     : League.Strings.Universal_String;
      Password : League.Strings.Universal_String) return Boolean
   is
      pragma Unreferenced (Self);

      Settings : League.Settings.Settings;
      Holder   : constant League.Holders.Holder :=
        Settings.Value ("users/password." & User);
   begin
      if League.Holders.Is_Empty (Holder) then
         return False;
      else
         return League.Holders.Element (Holder) = Password;
      end if;
   end Check_User_Password;

   ------------
   -- Do_Get --
   ------------

   overriding procedure Do_Get
     (Self     : in out OAuth2_Server_Servlet;
      Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class)
   is
      Client_Id : constant League.Strings.Universal_String :=
        Request.Get_Parameter (+"client_id");

      Response_Type : constant League.Strings.Universal_String :=
        Request.Get_Parameter (+"response_type");

      Settings : League.Settings.Settings;
   begin
      if not Settings.Contains ("oauth2/secret." & Client_Id)
        or else Response_Type /= +"code"  --  TODO check Redirect_URI
      then
         Response.Set_Status (Servlet.HTTP_Responses.Unauthorized);
         return;
      end if;

      Response.Set_Status (Servlet.HTTP_Responses.OK);
      Response.Set_Header (+"Cache-Control", +"no-cache");
      Response.Set_Content_Type (+"text/html");
      Response.Set_Character_Encoding (+"utf-8");

      Response.Get_Output_Stream.Write (Login_Page (Self, Request));
   end Do_Get;

   -------------
   -- Do_Post --
   -------------

   overriding procedure Do_Post
     (Self     : in out OAuth2_Server_Servlet;
      Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class)
   is
      Path_Info    : constant League.String_Vectors.Universal_String_Vector
        := Request.Get_Path_Info;
   begin
      if Path_Info.Length = 1 then
         if Path_Info (1) = +"sign-in" then
            Self.Do_Post_Sign_In (Request, Response);
            return;
         elsif Path_Info (1) = +"token-exchange" then
            Self.Do_Post_Token_Exchange (Request, Response);
            return;
         end if;
      end if;

      Response.Set_Status (Servlet.HTTP_Responses.Unauthorized);
   end Do_Post;

   ---------------------
   -- Do_Post_Sign_In --
   ---------------------

   procedure Do_Post_Sign_In
     (Self     : in out OAuth2_Server_Servlet'Class;
      Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class)
   is
      Settings  : League.Settings.Settings;

      Client_Id : constant League.Strings.Universal_String :=
        Request.Get_Parameter (+"client_id");

      Redirect_URI : constant League.Strings.Universal_String :=
        Request.Get_Parameter (+"redirect_uri");

      State : constant League.Strings.Universal_String :=
        Request.Get_Parameter (+"state");

      User : constant League.Strings.Universal_String :=
        Request.Get_Parameter (+"user");

      Password : constant League.Strings.Universal_String :=
        Request.Get_Parameter (+"password");

      Code : constant Authorization_Code := Self.Generate_Code;
      URI  : League.IRIs.IRI :=
        League.IRIs.From_Universal_String (Redirect_URI);
   begin
      if Settings.Contains ("oauth2/secret." & Client_Id)
        and then Self.Check_User_Password (User, Password)
      then
         Self.Codes.Include (Client_Id, Code);

         URI.Set_Query ("code=" & Code.Code & "&state=" & State);
         Response.Send_Redirect (URI);
      else
         Response.Set_Status (Servlet.HTTP_Responses.OK);
         Response.Set_Header (+"Cache-Control", +"no-cache");
         Response.Set_Content_Type (+"text/html");
         Response.Set_Character_Encoding (+"utf-8");

         Response.Get_Output_Stream.Write (Login_Page (Self, Request));
         Response.Set_Status (Servlet.HTTP_Responses.Unauthorized);
      end if;

   end Do_Post_Sign_In;

   ----------------------------
   -- Do_Post_Token_Exchange --
   ----------------------------

   procedure Do_Post_Token_Exchange
     (Self     : in out OAuth2_Server_Servlet'Class;
      Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class)
   is
      use type Ada.Calendar.Time;

      procedure Create_Token
        (Result  : out League.Strings.Universal_String;
         Subject : Wide_Wide_String;
         Expire  : Natural := 0);

      ------------------
      -- Create_Token --
      ------------------

      procedure Create_Token
        (Result  : out League.Strings.Universal_String;
         Subject : Wide_Wide_String;
         Expire  : Natural := 0)
      is
         Object : League.JSON.Objects.JSON_Object;
         Header : JWS.JOSE_Header;
         JWT    : JWS.JSON_Web_Signature;
         Value  : League.Holders.Universal_Integer;
         Epoch  : constant Ada.Calendar.Time :=
           Ada.Calendar.Formatting.Time_Of (1970, 1, 1, 0.0);

      begin
         Header.Set_Algorithm (+"HS256");

         if Expire > 0 then
            Value := League.Holders.Universal_Integer
              (Ada.Calendar.Clock - Epoch + Duration (Expire));
            Object.Insert (+"exp", League.JSON.Values.To_JSON_Value (Value));
         end if;

         Object.Insert (+"sub", League.JSON.Values.To_JSON_Value (+Subject));

         JWT.Create
           (Header,
            Object.To_JSON_Document.To_JSON.To_Stream_Element_Array,
            Self.Secret);

         Result := JWT.Compact_Serialization;
      end Create_Token;

      Expire    : constant := 6 * 60 * 60;
      Settings  : League.Settings.Settings;
      Text      : League.Strings.Universal_String;
      Reply     : League.JSON.Objects.JSON_Object;
      Now       : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Client_Id : constant League.Strings.Universal_String :=
        Request.Get_Parameter (+"client_id");
      Secret : constant League.Strings.Universal_String :=
        Request.Get_Parameter (+"client_secret");
      Grant : constant League.Strings.Universal_String :=
        Request.Get_Parameter (+"grant_type");
      Code : constant League.Strings.Universal_String :=
        Request.Get_Parameter (+"code");
      Token : constant League.Strings.Universal_String :=
        Request.Get_Parameter (+"refresh_token");
      Setting : constant League.Holders.Holder :=
        Settings.Value ("oauth2/secret." & Client_Id);

   begin
      Response.Set_Content_Type (+"application/json");
      Response.Set_Character_Encoding (+"utf-8");

      Reply.Insert
        (+"token_type", League.JSON.Values.To_JSON_Value (+"Bearer"));

      Reply.Insert (+"expires_in", League.JSON.Values.To_JSON_Value (Expire));

      Create_Token (Text, "user", Expire);

      Reply.Insert (+"access_token", League.JSON.Values.To_JSON_Value (Text));

      if Grant = +"authorization_code" then
         if Self.Codes.Contains (Client_Id)
           and then League.Holders.Element (Setting) = Secret
           and then Self.Codes (Client_Id).Code = Code
           and then Self.Codes (Client_Id).Expire >= Now
         then
            Self.Codes.Delete (Client_Id);

            Create_Token (Text, "refresh");

            Reply.Insert
              (+"refresh_token",
               League.JSON.Values.To_JSON_Value (Text));

            Response.Get_Output_Stream.Write
              (Reply.To_JSON_Document.To_JSON);

            Response.Set_Status (Servlet.HTTP_Responses.OK);

            return;
         end if;
      elsif Grant = +"refresh_token" then
         if not League.Holders.Is_Empty (Setting)
           and then League.Holders.Element (Setting) = Secret
         then
            declare
               JWT    : JWS.JSON_Web_Signature;
               Ok     : Boolean;
               Object : League.JSON.Objects.JSON_Object;
            begin
               JWT.Validate_Compact_Serialization (Token, Self.Secret, Ok);

               if Ok then
                  Object := JWT.Payload_Object;

                  if Object.Value (+"sub").To_String = +"refresh" then
                     Response.Get_Output_Stream.Write
                       (Reply.To_JSON_Document.To_JSON);

                     Response.Set_Status (Servlet.HTTP_Responses.OK);

                     return;
                  end if;
               end if;
            end;
         end if;
      end if;

      Response.Get_Output_Stream.Write (+"{'error': 'invalid_grant'}");
      Response.Set_Status (Servlet.HTTP_Responses.Bad_Request);
   end Do_Post_Token_Exchange;

   --------------------
   -- Generate_Token --
   --------------------

   function Generate_Code (Self : OAuth2_Server_Servlet'Class)
     return Authorization_Code
   is
      use type Ada.Calendar.Time;

      Data : League.Stream_Element_Vectors.Stream_Element_Vector;
   begin
      for J in 1 .. 12 loop
         Data.Append (Stream_Element_Random.Random (Self.Random));
      end loop;

      return (Code   => League.Base_Codecs.To_Base_64_URL (Data),
              Expire => Ada.Calendar.Clock + 600.0);
   end Generate_Code;

   ----------------------
   -- Get_Servlet_Info --
   ----------------------

   overriding function Get_Servlet_Info
     (Self : OAuth2_Server_Servlet) return League.Strings.Universal_String
   is
      pragma Unreferenced (Self);
   begin
      return +"OAuth2 Servlet Servlet";
   end Get_Servlet_Info;

   -----------------
   -- Instantiate --
   -----------------

   overriding function Instantiate
     (Parameters : not null access Servlet.Generic_Servlets
        .Instantiation_Parameters'Class)
      return OAuth2_Server_Servlet
   is
      pragma Unreferenced (Parameters);

      Settings : League.Settings.Settings;

      Holder   : constant League.Holders.Holder :=
        Settings.Value (+"server/secret");

      Text     : constant League.Strings.Universal_String :=
        League.Holders.Element (Holder);
   begin
      return Result : OAuth2_Server_Servlet do
         Stream_Element_Random.Reset (Result.Random);
         Result.Secret :=
           League.Base_Codecs.From_Base_64 (Text).To_Stream_Element_Array;
      end return;
   end Instantiate;

   ----------------
   -- Login_Page --
   ----------------

   function Login_Page
     (Self    : OAuth2_Server_Servlet'Class;
      Request : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class)
      return League.Strings.Universal_String
   is
      pragma Unreferenced (Self);

      Settings : League.Settings.Settings;
      Context   : constant access Servlet.Contexts.Servlet_Context'Class
        := Request.Get_Servlet_Context;

      Client_Id : constant League.Strings.Universal_String :=
        Request.Get_Parameter (+"client_id");

      Redirect_URI : constant League.Strings.Universal_String :=
        Request.Get_Parameter (+"redirect_uri");

      Scope : constant League.Strings.Universal_String :=
        Request.Get_Parameter (+"scope");

      State : constant League.Strings.Universal_String :=
        Request.Get_Parameter (+"state");

      App : constant League.Strings.Universal_String :=
        League.Holders.Element
          (Settings.Value ("oauth2/name." & Client_Id));

      XHTML     : constant League.Strings.Universal_String :=
        +"/password.xhtml.tmpl";
      Input     : aliased XML.SAX.Input_Sources.Streams.Files
        .File_Input_Source;
      Reader    : aliased XML.SAX.Simple_Readers.Simple_Reader;
      Filter    : aliased XML.Templates.Processors.Template_Processor;
      Writer    : aliased XML.SAX.Pretty_Writers.XML_Pretty_Writer;
      Output    : aliased XML.SAX.Output_Destinations.Strings
        .String_Output_Destination;
   begin
      --  Set template input
      Input.Open_By_File_Name (Context.Get_Real_Path (XHTML));

      --  Configure reader
      Reader.Set_Input_Source (Input'Unchecked_Access);
      Reader.Set_Content_Handler (Filter'Unchecked_Access);
      Reader.Set_Lexical_Handler (Filter'Unchecked_Access);

      --  Configure template processor
      Filter.Set_Content_Handler (Writer'Unchecked_Access);
      Filter.Set_Lexical_Handler (Writer'Unchecked_Access);

      --  Bind parameters
      Filter.Set_Parameter (+"app", League.Holders.To_Holder (App));
      Filter.Set_Parameter
        (+"client_id", League.Holders.To_Holder (Client_Id));
      Filter.Set_Parameter
        (+"redirect_uri", League.Holders.To_Holder (Redirect_URI));
      Filter.Set_Parameter (+"state", League.Holders.To_Holder (State));
      Filter.Set_Parameter (+"scope", League.Holders.To_Holder (Scope));

      --  Configure XML writer.

      Writer.Set_Output_Destination (Output'Unchecked_Access);
      --  Process template
      Reader.Parse;

      return Output.Get_Text;
   end Login_Page;

end Servlet.OAuth2_Server;
