{------------------------------------------------------------------------------}
{ Common compiler defines                                                      }
{ (remove the dot in front of a define to enable it)                           }
{------------------------------------------------------------------------------}

{$B-,H+} // defaults are short evaluation of boolean values and long strings

{.$DEFINE SYN_DEVELOPMENT_CHECKS} // additional tests for debugging
{$IFDEF SYN_DEVELOPMENT_CHECKS}
  {$R+,Q+,S+,T+}
{$ENDIF}

{------------------------------------------------------------------------------}
{ Pull in all defines from SynEditJedi.inc (must be done after the common      }
{ compiler defines to  work correctly). Use SynEdit-prefix to avoid problems   }
{ with other versions of jedi.inc in the search-path.                          }
{------------------------------------------------------------------------------}

{$I SynEditJedi.inc}

{------------------------------------------------------------------------------}
{  Please change this to suit your needs (to activate an option remove the dot }
{  in front of a DEFINE)                                                       }
{------------------------------------------------------------------------------}

// "Heredoc" syntax highlighting
// If you enable the following statement and use highlighter(s) that have
// support for "Heredoc" strings as scheme(s) in SynMultiSyn, you must
// implement your own SynMultiSyn OnCustomRange event handler in order to
// properly store Range State information
{.$DEFINE SYN_HEREDOC}

// Turn this off if you don't need complex script support, since it is slower
{.$DEFINE SYN_UNISCRIBE}

// $Id: SynEdit.inc,v 1.16.2.19 2009/06/14 13:41:44 maelh Exp $
