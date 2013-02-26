%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    Definition of analog channel config
%%% @end
%%% Created : 16 Dec 2012 by Tony Rogvall <tony@rogvall.se>

-ifndef(__ANALOG_HRL__).
-define(__ANALOG_HRL__, true).

-define(UPPER_LIMIT_EXCEEDED,                16#01).
-define(BELOW_LOWER_LIMIT,                   16#02).
-define(CHANGED_BY_MORE_THAN_DELTA,          16#04).
-define(CHANGED_BY_MORE_THAN_NEGATIVE_DELTA, 16#08).
-define(CHANGED_BY_MORE_THAN_POSITIVE_DELTA, 16#10).
-define(LIMIT_BITS, 16#03).
-define(DELTA_BITS, 16#1C).

-record(analog_config,
	{
	  index,                %% pin / config index
	  name,                 %% name label
	  trigger = 0,          %% trigger selection
	  scale = 1,            %% scale  (X*scale + offst)
	  offset = 0,           %% offset
	  upper_limit = 0,     %% upper_limit used by upper-limit-exceeded check
	  lower_limit = 0,      %% lower_limit used by below-lower-limit check
	  delta = 0,            %% changed more than delta
	  negative_delta,       %% changed more than negative delta
	  positive_delta,       %% changed more than positive delta
	  sample_frequency,     %% number of times per second to sample value
	  max_transmit_frequency %% maximum frequency to send values
	}).

-record(analog,
	{
	  inhibit_us,       %% calculate time to inhibit transmit 
	  value,            %% last sampled value
	  scaled_value,     %% last scaled sample
	  timestamp,        %% last timestamp sample sent
	  inhibit = 0,      %% inhibit timestamp
	  mask = 0          %% previous trigger mask
	}).
	  
-endif.
