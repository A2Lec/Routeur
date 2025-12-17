
package IP is

    type T_IP is limited private;
    type T_Masque is limited private;

    -- masque une adresse pour la faire corresondre à un masque
    procedure Masquer_adresse (Adresse : in out T_IP; Masque : in T_Masque);


    -- masque un bit de l'adresse
    procedure Masquer_bit (Adresse : in out T_IP);

    -- détermine si une adresse correspond à une autre selon un masque / ou juste tester égalité de 2 adresse ?

private

    type T_IP is Unbounded_String;
    type T_Masque is Unbounded_String;

end IP;_
