with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Adresse_IP;            use Adresse_IP;
with Trie;                  use Trie;

package Cache_Trie is

	type T_Politique is (FIFO, LRU, LFU);
	-- type T_Strategie is (FIFO, LRU, LFU);
	
	type T_Cache is limited private;
	
	procedure Initialiser(
		Cache : out T_Cache;
		Taille_Max : in Integer;
		Politique : in T_Politique);
	
	function Est_Vide(Cache : in T_Cache) return Boolean;
	
	function Est_Plein(Cache : in T_Cache) return Boolean;
	
	function Taille(Cache : in T_Cache) return Integer;
	
	function Taille_Max(Cache : in T_Cache) return Integer;
	
	function Rechercher(
		Cache : in out T_Cache;
		Adresse : in T_IP;
		Interface_Sortie : out Unbounded_String) return Boolean;
	
	procedure Ajouter(
		Cache : in out T_Cache;
		Adresse : in T_IP;
		Longueur_Prefixe : in Integer;
		Interface_Sortie : in Unbounded_String);
	
	procedure Supprimer(
		Cache : in out T_Cache;
		Adresse : in T_IP;
		Longueur_Prefixe : in Integer);
	
	procedure Vider(Cache : in out T_Cache);
	
	procedure Afficher(Cache : in T_Cache);
	
	procedure Afficher_Stats(Cache : in T_Cache);
	
	function Nb_Demandes(Cache : in T_Cache) return Integer;
	
	function Nb_Defauts(Cache : in T_Cache) return Integer;
	
	function Taux_Defaut(Cache : in T_Cache) return Float;
	
	procedure Reinitialiser_Stats(Cache : in out T_Cache);

private

	type T_Info_Route is record
		Adresse : T_IP;
		Longueur_Prefixe : Integer;
		Interface_Sortie : Unbounded_String;
		Horodatage : Integer;
		Compteur : Integer;
	end record;
	
	type T_Tab_Routes is array (1..1000) of T_Info_Route;
	
	type T_Cache is record
		Trie_Routes : Trie.T_Trie;
		Routes : T_Tab_Routes;
		Taille_Actuelle : Integer;
		Taille_Maximale : Integer;
		Politique_Cache : T_Politique;
		Nb_Demandes_Total : Integer;
		Nb_Defauts_Cache : Integer;
		Horloge : Integer;
	end record;

end Cache_Trie;
