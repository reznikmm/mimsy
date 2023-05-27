--  SPDX-FileCopyrightText: 2016-2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Text_IO;

with AWFC.Static_Resource_Servlets;
pragma Unreferenced (AWFC.Static_Resource_Servlets);
with Servlet.OAuth;
--  with Servlet.Users;
--  pragma Unreferenced (Servlet.Users);
--  with Servlet.Telegram;
--  with Servlet.Viber;
--  with Servlet.Forum;
--  pragma Unreferenced (Servlet.Forum);
--  with Servlet.Pastebin;
--  pragma Unreferenced (Servlet.Pastebin);
--  with Servlet.Compile;
--  pragma Unreferenced (Servlet.Compile);
with Servlet.Game_Solutions;
pragma Unreferenced (Servlet.Game_Solutions);
with Servlet.Game_Missions;
pragma Unreferenced (Servlet.Game_Missions);
with Servlet.Game_Stations;
pragma Unreferenced (Servlet.Game_Stations);

--  with Axe.Wiki_View_Servlets;

with Matreshka.Internals.SQL_Drivers.PostgreSQL.Factory;
pragma Unreferenced (Matreshka.Internals.SQL_Drivers.PostgreSQL.Factory);

--  with Axe.Events.Logs;
--  with XMPP.Sessions;
--  with League.Holders;
--  with League.Settings;
with League.Strings;
with Servlet.Generic_Servlets;
with Mimsy.Sessions.Managers;
with Spikedog.HTTP_Session_Managers;
with Spikedog.Servlet_Contexts;

package body Mimsy.Startup is

   ----------------
   -- On_Startup --
   ----------------

   overriding procedure On_Startup
     (Self    : in out Servlet_Container_Initializer;
      Context : in out Servlet.Contexts.Servlet_Context'Class)
   is
      pragma Unreferenced (Self);

      function "+"
        (Item : Wide_Wide_String) return League.Strings.Universal_String
         renames League.Strings.To_Universal_String;

      Manager  : constant Sessions.Managers.HTTP_Session_Manager_Access :=
        new Sessions.Managers.HTTP_Session_Manager;

      --  Log_Writer : constant Axe.Events.Logs.Event_Log_Writer_Access
      --    := new Axe.Events.Logs.Event_Log_Writer;
      --
      Dummy : aliased Servlet.Generic_Servlets.Instantiation_Parameters;

      OAuth_Servlet : constant Servlet.OAuth.OAuth_Servlet_Access :=
        new Servlet.OAuth.OAuth_Servlet'
          (Servlet.OAuth.Instantiate (Dummy'Unchecked_Access));

   --  Telegram_Servlet : constant Servlet.Telegram.Telegram_Servlet_Access :=
      --    new Servlet.Telegram.Telegram_Servlet'
      --      (Servlet.Telegram.Instantiate (Dummy'Unchecked_Access));
      --
      --  Viber_Servlet : constant Servlet.Viber.Viber_Servlet_Access :=
      --    new Servlet.Viber.Viber_Servlet'
      --      (Servlet.Viber.Instantiate (Dummy'Unchecked_Access));
      --
   --  Wiki_Servlet : constant Axe.Wiki_View_Servlets.Wiki_View_Servlet_Access
      --    := new Axe.Wiki_View_Servlets.Wiki_View_Servlet;
      --
      --  Settings  : League.Settings.Settings;
      --  Telegram  : constant League.Strings.Universal_String :=
      --    League.Holders.Element (Settings.Value (+"/telegram/token"));
      --  Viber     : constant League.Strings.Universal_String :=
      --    League.Holders.Element (Settings.Value (+"/viber/token"));
   begin
      Manager.Initialize;

      Spikedog.Servlet_Contexts.Spikedog_Servlet_Context'Class
        (Context).Set_Session_Manager
          (Spikedog.HTTP_Session_Managers.HTTP_Session_Manager_Access
             (Manager));

      --  XMPP.Sessions.Initialize;
      --
      --  Log_Writer.Initialize
      --    (File     => Context.Get_Real_Path (+"/news.wiki"),
      --     Password => Context.Get_Real_Path (+"/password/ada_ru"),
      --     Telegram => Telegram,
      --     Viber    => Viber);
      --
      --  Wiki_Servlet.Set_Event_Listener (Log_Writer);
      --  Context.Add_Servlet (+"WikiRendering", Wiki_Servlet);
      --
      OAuth_Servlet.Set_Handler (Manager);
      Context.Add_Servlet (+"OAuth", OAuth_Servlet);
      --
      --  Telegram_Servlet.Initialize (Telegram);
      --  Telegram_Servlet.Set_Listener (Log_Writer);
      --  Context.Add_Servlet (+"Telegram", Telegram_Servlet);
      --
      --  Viber_Servlet.Initialize (Viber);
      --  Viber_Servlet.Set_Listener (Log_Writer);
      --  Context.Add_Servlet (+"Viber", Viber_Servlet);

      Ada.Text_IO.Put_Line ("I'm here!");
      --  TODO: /arm/*
      --  TODO: set_password.html
   end On_Startup;

end Mimsy.Startup;