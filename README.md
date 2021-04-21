# tataru
A discord scheduling bot

# Running

The following environment variables need to be set:

* DISCORD_HOST - the discord host (should just be discord.com)
* DISCORD_TOKEN - the bot token
* TATARU_AUTHORIZED - a ; separated list of people with tataru admin access
* TATARU_PLUGINS - a ; separated list of plugins to load at launch

If you are using the scheduler plugin you need to set the following:

* TATARU_SCHEDULE_CHANNEL - a channel id to post scheduler alerts to
