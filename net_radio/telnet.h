#ifndef _TELNET_
#define _TELNET_

#include <unistd.h>
#include <netinet/ip.h>
#include <string>

// Maksymalny rozmiar pojedyńczej wiadomości, przekroczenie go powoduje błąd
const int MAX_MESSAGE_SIZE = 255;
// Maksymalna ilość połączeń
const int MAX_CONNECTIONS = SOMAXCONN;

/* Tworzy gniazdo nasłuchujące
 * 
 * Jeśli port == 0 to wybiera sobie dowolny i wpisuje do zmiennej
 * 
 * Przyjmuje wartość socketa lub -1
 */
int telnet_create_listening_socket(uint16_t& port);

/* Akceptuje przychodzącą sesję telneta na gnieździe sock
 * i zwraca gniazdo nowego połączenia. Jeżeli wystąpił błąd to nie trzeba
 * zamykać socketa funkcją telnet_close.
 */
int telnet_accept(int sock);

/* Zamyka połączenie na podanej sesji telneta
 */
void telnet_close(int sock);

/* Wypełnia bufor na wiadomości informacjami z podanego socketa.
 * Zwraca -1 w przypadku gdy bufor nie istnieje lub odczyt się nie powiódł.
 * 0 gdy połączenie zostało zamknięte
 * lub ilość wczytanych znaków w przeciwnym przypadku
 */
int telnet_fill_buffer(int socket);

/* Sprawdza, czy jakaś wiadomość oczekuje na danym sockecie lub bufor jest
 * przepełniony
 */
bool telnet_is_message(int sock);

/* Strukturka z przekazanym poleceniem.
 * 
 * Typy poleceń: INVALID, TOOLONG oznaczają błąd
 * 
 * START:
 * - machine - nazwa maszyny na której ma być uruchomiony player
 * - params1 - polecenia playera
 * - params2 - port na jakim player nasłuchuje na polecenia
 * 
 * ASK:
 * - machine - ID playera
 * - params1 - akcja do wykonania
 * 
 * AT:
 * - machine - maszyna w sieci
 * - params1 - godzina
 * - params2 - minuta
 * - params3 - przez ile minut ma działać
 * - params4 - polecenia playera
 * - params5 - port na jakim player nasłuchuje na polecenia
 */
struct TelnetCommand
{
	enum class Type {INVALID, TOOLONG, START, ASK, AT} type;
	std::string machine;
	std::string params1, params2, params3, params4, params5;
};

/* Czyta wiadomość z podanego socketa i zwraca wypełnioną strukturę
 * z parametrami odebranego polecenia. 
 */
TelnetCommand telnet_read_message(int sock);

#endif
