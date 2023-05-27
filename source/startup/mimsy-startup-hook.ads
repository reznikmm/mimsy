--  SPDX-FileCopyrightText: 2016-2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Spikedog.Generic_Application_Initializer;

package Mimsy.Startup.Hook is
  new Spikedog.Generic_Application_Initializer
       (Mimsy.Startup.Servlet_Container_Initializer);
