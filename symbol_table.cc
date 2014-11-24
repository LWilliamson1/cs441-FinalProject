/***************************************************************************
 *   Copyright (C) 2008 by Jody Larsen                                     *
 *   jody@dreamfrog.com                                                    *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/

#include <iostream>
#include <string>
#include <map>
#include <list>
#include <cstdlib>
#include "symbol_table.h"

using namespace std;

//A list to hold the symbol table stack
list < map < string, symbol_entry > >symbol_table_stack;

//An empty symbol table to push onto the symbol table stack
//Since we are passing objects by value, this is copied on each stack push
map < string, symbol_entry > empty_symbol_table;

void
printSymbolTable ()
{
  //A pointer to the current symbol table
  map < string, symbol_entry > *curr_symbol_table;

  //Used to track the current block level
  int symbol_table_level = symbol_table_stack.size ();

  cout << "currently in block level " << (symbol_table_level - 1) << endl;

  //If the symbol table stack isn't empty, print it
  if (!symbol_table_stack.empty ())
    {
      //Get an iterator to loop through the entire stack from lowest level
      //block to highest level block
      for (list < map < string, symbol_entry > >::iterator i_symbol_table =
	   symbol_table_stack.begin ();
	   i_symbol_table != symbol_table_stack.end (); i_symbol_table++)
	{
	  symbol_table_level--;
	  curr_symbol_table = &(*i_symbol_table);

	  //Print the symbol table
	  if (!curr_symbol_table->empty ())
	    {
	      cout << "\t" << "Symbol Table, Level " << symbol_table_level <<
		endl;
	      map < string, symbol_entry >::iterator cur_entry =
		curr_symbol_table->begin ();
	      while (cur_entry != curr_symbol_table->end ())
		{
		  symbol_entry entry = (*curr_symbol_table)[cur_entry->first];
		  string type_name = "Unknown";
		  string type_value = "Empty";
		  switch (entry.value.type)
		    {
		    case TYPE_INT:
		      type_name = "int";
			  type_value = entry.value._int;
		      break;
		    case TYPE_DOUBLE:
		      type_name = "double";
			  type_value = entry.value._double;
		      break;
		    }
		  std::cout << "\t\t" <<
		    "name(" << entry.name << ") " <<
		    "addr(" << entry.addr << ") " <<
		    "blk_level(" << entry.blk_level << ") " <<
		    "offset(" << entry.offset << ") " <<
		    "type(" << type_name << ") " <<
		    "size(" << entry.size << ") " << endl;
		  ++cur_entry;
		}
	      cout << endl;
	    }
	}
    }
}

void
popSymbolTable ()
{
  symbol_table_stack.pop_front ();
}

void
pushSymbolTable ()
{
  //Allocate a new symbol table and push it onto the symbol table stack
  //Note that empty_symbol_table is copied during the push as this is
  //pass by value
  symbol_table_stack.push_front (empty_symbol_table);
}

void
addSymbol (symbol_entry * new_entry, int lineno)
{
  //A pointer to the current symbol table
  map < string, symbol_entry > *curr_symbol_table;

  //Used to track the current block level
  int symbol_table_level = symbol_table_stack.size ();

  //Get an iterator to loop through the entire stack from lowest level
  //block to highest level block
//  for (list < map < string,
//       symbol_entry > >::iterator i_symbol_table =
//       symbol_table_stack.begin ();
//       i_symbol_table != symbol_table_stack.end (); i_symbol_table++)
//    {
//      symbol_table_level--;
//      curr_symbol_table = &(*i_symbol_table);
//
//      //if variable already seen, print a warning
//      if ((*curr_symbol_table).count (new_entry->name) > 0)
//	{
//	  cerr << " Multiple declarations of " << new_entry->name << endl;
//	  exit (1);
//	}
 //   }

  //Get symbol table for current block
  curr_symbol_table = &(*(symbol_table_stack.begin ()));

  if ((*curr_symbol_table).count (new_entry->name) > 0)
  {
    cerr << "Multple declarations of " << new_entry->name << " near line " << lineno << endl;
    exit(1);
  }

  //Set offset
  new_entry->offset = getCurrentSymbolTableSize() + 3;

  //Set block
  new_entry->blk_level = symbol_table_stack.size () - 1;
  
  //Add the entry to the current symbol table
  (*curr_symbol_table)[new_entry->name] = *new_entry;
  return;
}

symbol_entry *
getSymbolEntryByRelAddr (int rel_blk_level, int offset)
{
  symbol_entry *entry;

  //A pointer to the current symbol table
  map < string, symbol_entry > *curr_symbol_table;

  //If the symbol table stack isn't empty, iterate through it
  if (!symbol_table_stack.empty ())
    {

      //Get an iterator to loop through the entire stack from lowest level
      //block to highest level block
      list < map < string, symbol_entry > >::iterator i_symbol_table =
		symbol_table_stack.begin ();
      for(int i = 0; i < rel_blk_level; i++) {
	i_symbol_table++;
      }
          curr_symbol_table = &(*i_symbol_table);

	  if (!curr_symbol_table->empty ())
	    {
	      map < string, symbol_entry >::iterator cur_entry =
		curr_symbol_table->begin ();
	      while (cur_entry != curr_symbol_table->end ())
		{
		  entry = &(*curr_symbol_table)[cur_entry->first];
		  if(entry->offset == offset)
		  {
		    return entry;
		  }
		  ++cur_entry;
		}
	    }
    }
    //If variable not found, print error and exit
    cerr << "Variable " << rel_blk_level << ", " << offset << " not declared." << endl;
    printSymbolTable();
    exit (1);
}

symbol_entry *
getSymbolEntry (const char *yytext)
{
  //A pointer to the current symbol table
  map < string, symbol_entry > *curr_symbol_table;

  //Used to track the current block level
  int symbol_table_level = symbol_table_stack.size ();

  //Get an iterator to loop through the entire stack from lowest level
  //block to highest level block
  for (list < map < string,
       symbol_entry > >::iterator i_symbol_table =
       symbol_table_stack.begin ();
       i_symbol_table != symbol_table_stack.end (); i_symbol_table++)
    {
      symbol_table_level--;
      curr_symbol_table = &(*i_symbol_table);

      //if variable is found, return it
      if ((*curr_symbol_table).count (yytext) > 0)
	{
	  return &((*curr_symbol_table)[yytext]);
	}
    }

  //If variable not found, print error and exit
  /* removed jwj
  cerr << "Variable " << yytext << " not declared." << endl;
  exit (1);
  */
  return 0;
}

int
getTotalSymbolTableSize ()
{
  //An interator for the symbol table stack
  list < map < string, symbol_entry > >::iterator i_symbol_table =
	symbol_table_stack.begin ();

  int size = 0;
  while(i_symbol_table != symbol_table_stack.end()) {

  //A pointer to the current symbol table
  map < string, symbol_entry > *curr_symbol_table =
    &(*i_symbol_table);

  if (!curr_symbol_table->empty ())
    {
      map < string, symbol_entry >::iterator cur_entry =
	curr_symbol_table->begin ();
      while (cur_entry != curr_symbol_table->end ())
	{
	  symbol_entry entry = (*curr_symbol_table)[cur_entry->first];
	  size += entry.size;
	  cur_entry++;
	}
    }
    i_symbol_table++;
  }

  return size;
}

int
getSymbolTableSize (int relative_level)
{
  //An interator for the symbol table stack
  list < map < string, symbol_entry > >::iterator i_symbol_table =
	symbol_table_stack.begin ();

  for(int i = 0; i < relative_level; i++) {
	i_symbol_table++;
  }

  //A pointer to the current symbol table
  map < string, symbol_entry > *curr_symbol_table =
    &(*i_symbol_table);

  int size = 0;
  if (!curr_symbol_table->empty ())
    {
      map < string, symbol_entry >::iterator cur_entry =
	curr_symbol_table->begin ();
      while (cur_entry != curr_symbol_table->end ())
	{
	  symbol_entry entry = (*curr_symbol_table)[cur_entry->first];
	  size += entry.size;
	  cur_entry++;
	}
    }

  return size;
}

int
getCurrentSymbolTableSize ()
{
	return getSymbolTableSize(0);
}

int getCurrentLevel() {
	return symbol_table_stack.size () - 1;
}
