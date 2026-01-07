with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Strings;               use Ada.Strings;

with Adresse_IP; use Adresse_IP;

package Trie is
	
	type T_Trie is limited private;

	
	function EstVide(trie : in T_Trie) return Boolean;
	
	function EstFeuille(trie : in T_Trie) return Boolean;
	
	function Taille(trie : in T_Trie) return Integer;

	procedure AjouterAdresse(trie : out T_Trie; adresse : in T_IP);

	function AdresseExiste(trie : in T_Trie; adresse: in T_IP) return Boolean;

	procedure SupprimerAdresse(trie : in out T_Trie; adresse : in T_IP);
	
	procedure Affiche_debug(trie : in T_Trie);

private
	type T_Enum_IP is range 0..11;
	type T_Noeuds is array (T_Enum_IP) of T_Trie;

	type T_Cellule_Trie is record
		Noeuds : T_Noeuds;
		valeur : Character;
	end record;	

	type T_Trie is access T_Cellule_Trie;	

end Trie;
