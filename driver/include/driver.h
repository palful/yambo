/*
         Copyright (C) 2000-2019 the YAMBO team
               http://www.yambo-code.org
 
  Authors (see AUTHORS file for details): AM
  
  This file is distributed under the terms of the GNU 
  General Public License. You can redistribute it and/or 
  modify it under the terms of the GNU General Public 
  License as published by the Free Software Foundation; 
  either version 2, or (at your option) any later version.
 
  This program is distributed in the hope that it will 
  be useful, but WITHOUT ANY WARRANTY; without even the 
  implied warranty of MERCHANTABILITY or FITNESS FOR A 
  PARTICULAR PURPOSE.  See the GNU General Public License 
  for more details.
 
  You should have received a copy of the GNU General Public 
  License along with this program; if not, write to the Free 
  Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
  MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
*/

struct yambo_seed_struct command_line(int argc, char *argv[],struct n_options_struct opts[], struct tool_struct t, int *use_editor, int *use_mpi);

void input_file(struct yambo_seed_struct y,struct tool_struct t, int use_editor);

void launcher(int np, int pid, struct yambo_seed_struct y,int *use_editor , int *use_mpi );

void load_environments(char *file_name, char* editor);

void options_maker(struct n_options_struct n_options[]);

struct tool_struct tool_init( );

void usage(options_struct *opts,int verbose, struct tool_struct t);

void title(FILE *file_name,char *cmnt, struct tool_struct t);

char *running_tool();

#if defined _FORTRAN_US
 extern int get_the_version_
#else
 extern int get_the_version
#endif
(int *,int *,int *, int *, char *);

