import json
import time
import requests
from rich.console import Console
from rich.table import Table
from rich.live import Live
from rich.panel import Panel
from rich.text import Text
from rich.theme import Theme

# Custom theme for Fluxer Debug
fluxer_theme = Theme({
    "info": "bold green",
    "warn": "bold yellow",
    "error": "bold red",
    "debug": "bold blue",
    "service": "bold magenta",
    "time": "dim cyan",
})

console = Console(theme=fluxer_theme)

def get_level_style(level):
    level = level.upper()
    if level == "INFO": return "info"
    if level == "WARN": return "warn"
    if level == "ERROR": return "error"
    if level == "DEBUG": return "debug"
    return "white"

def format_rest(rest):
    if not rest:
        return ""
    try:
        return "\n" + json.dumps(rest, indent=2)
    except:
        return str(rest)

def run_terminal():
    url = "http://localhost:8773/_debug/events"
    
    console.print(Panel("[bold purple]Fluxer Debug Terminal[/bold purple]\n[dim]Connecting to proxy at http://localhost:8773...[/dim]", 
                        border_style="purple"))

    while True:
        try:
            with requests.get(url, stream=True, timeout=None) as response:
                if response.status_code != 200:
                    console.print(f"[bold red]Error:[/] Proxy returned status {response.status_code}. Retrying in 3s...")
                    time.sleep(3)
                    continue

                console.print("[bold green]CONNECTED[/bold green] - Streaming live logs...\n")
                
                for line in response.iter_lines():
                    if line:
                        decoded_line = line.decode('utf-8')
                        if decoded_line.startswith('data: '):
                            try:
                                data = json.loads(decoded_line[6:])
                                level = data.get('level', 'INFO').upper()
                                service = data.get('service', 'unknown')
                                msg = data.get('msg', '')
                                timestamp = data.get('time', '')
                                
                                # Extract metadata
                                rest = {k: v for k, v in data.items() if k not in ['level', 'service', 'msg', 'time']}
                                
                                # Format output
                                time_str = timestamp.split('T')[-1].split('.')[0] if timestamp else "??:??:??"
                                
                                level_tag = Text(f" {level:5} ", style=get_level_style(level))
                                service_tag = Text(f" {service:15} ", style="service")
                                time_tag = Text(f" {time_str} ", style="time")
                                
                                meta_str = format_rest(rest)
                                
                                console.print(time_tag, level_tag, service_tag, Text(msg), style="white")
                                if meta_str:
                                    console.print(Text(meta_str, style="dim white"))
                                
                            except json.JSONDecodeError:
                                pass
                            except Exception as e:
                                console.print(f"[dim red]Parse Error: {e}[/]")

        except requests.exceptions.ConnectionError:
            console.print("[dim red]Connection Refused. Is the App Proxy running? Retrying in 3s...[/]")
            time.sleep(3)
        except Exception as e:
            console.print(f"[bold red]Unexpected Error:[/] {e}. Retrying in 5s...")
            time.sleep(5)

if __name__ == "__main__":
    try:
        run_terminal()
    except KeyboardInterrupt:
        console.print("\n[bold yellow]Exiting Debug Terminal...[/]")
