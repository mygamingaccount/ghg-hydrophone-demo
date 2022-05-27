declare name "GHG Compensator component";
import("stdfaust.lib");

// INPUT PARAMETERS
// Number of hydrophone elements.
// GHG had 2 groups of 24 hydrophones on each side
nElements = 24;
// Total length of a hydrophone group in meters.
// A longer group improves low-frequency beam width but limits the highest usable frequency
glen = 2.5;
// Speed of sound in m/s for calculating the delay
c = 1465;

// Compensator created using a simple delay line
// The Kriegsmarine stripline compensators used LC lowpass delay lines which had 17us group delay at DC.
// At 48000Hz one sample gives us 20.83us delay. I expect the performance to be very similar.
// TODO: Listening test at 58.8kHz sample rate to compare the performance of 17us vs 20.83us delays
compensator = si.bus(nElements) : par(n, nElements, stripline_delay(n)) :> _;

stripline_delay(n) = de.delay(maxdelay,strip_time)
with {
    maxdelay = ma.SR*glen/c;
    strip_time = maxdelay * abs(cos(brg)) * index/nElements;
    index = nElements-n-1, n: select2(cos(brg)<0);
    brg = ghg_dial * ma.PI / 180;
};

// For use with the Underwater Telegraph, a rectifier tube was added into the signal path according to the RN report on U-570
rectifier = _ <: _ , abs : select2(hslider("Rectifier[style:radio{'Off':0;'On':1}]",0,0,1,1)) : _;

ghg_dial = hslider("Compensator bearing",0,-180,180,0.001);

hplist =
    hslider(
            "Highpass[style:radio{'-':100;'500 Hz':500;'1000 Hz':1000;'3000 Hz':3000;'6000 Hz':6000;'10000 Hz':10000};]",
            100,10,10000,1
    );
highpass_switch = fi.highpass(8,hplist);

// Demonstration process configured for 6 sound sources, 2 outputs for stereo listening. 2nd order highpass represents the 
process = _  <: compensator : rectifier : highpass_switch : fi.lowpass(10,7000) / nElements;
