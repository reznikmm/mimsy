--  SPDX-FileCopyrightText: 2016-2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Text_IO;

with AWFC.Static_Resource_Servlets;
pragma Unreferenced (AWFC.Static_Resource_Servlets);
with Servlet.OAuth;
with Servlet.OAuth2_Server;
pragma Unreferenced (Servlet.OAuth2_Server);
with Servlet.Users;
pragma Unreferenced (Servlet.Users);
with Servlet.Compile;
pragma Unreferenced (Servlet.Compile);
with Servlet.Game_Solutions;
pragma Unreferenced (Servlet.Game_Solutions);
with Servlet.Game_Missions;
pragma Unreferenced (Servlet.Game_Missions);
with Servlet.Game_Stations;
pragma Unreferenced (Servlet.Game_Stations);

with Matreshka.Internals.SQL_Drivers.PostgreSQL.Factory;
pragma Unreferenced (Matreshka.Internals.SQL_Drivers.PostgreSQL.Factory);

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

      Dummy : aliased Servlet.Generic_Servlets.Instantiation_Parameters;

      OAuth_Servlet : constant Servlet.OAuth.OAuth_Servlet_Access :=
        new Servlet.OAuth.OAuth_Servlet'
          (Servlet.OAuth.Instantiate (Dummy'Unchecked_Access));

   begin
      Manager.Initialize;

      Spikedog.Servlet_Contexts.Spikedog_Servlet_Context'Class
        (Context).Set_Session_Manager
          (Spikedog.HTTP_Session_Managers.HTTP_Session_Manager_Access
             (Manager));

      OAuth_Servlet.Set_Handler (Manager);
      Context.Add_Servlet (+"OAuth", OAuth_Servlet);

      Ada.Text_IO.Put_Line ("I'm here!");
   end On_Startup;

end Mimsy.Startup;
