package net.soundmining

import net.soundmining.modular.ModularInstrument.{AudioInstrument, ControlInstrument, StaticAudioBusInstrument}
import net.soundmining.modular.ModularSynth
import net.soundmining.modular.ModularSynth.{amModulate, bandPassFilter, bandRejectFilter, controlMultiply, highPassFilter, lineControl, lowPassFilter, monoDelay, panning, pulseOsc, relativePercControl, relativeThreeBlockcontrol, ringModulate, sawOsc, sineControl, sineOsc, staticAudioBus, staticControl, triangleOsc}
import net.soundmining.sound.{SoundPlay, SoundPlays}
import net.soundmining.synth.Instrument.{EFFECT, TAIL_ACTION}
import net.soundmining.synth.SuperColliderClient.loadDir
import net.soundmining.synth.Utils.absoluteTimeToMillis
import net.soundmining.synth.{Instrument, SuperColliderClient}

import scala.collection.mutable

object ConcreteMusic8 {

  implicit val client: SuperColliderClient = SuperColliderClient()
  val SOUND_DIR = "/Users/danielstahl/Documents/Music/Pieces/Concrete Music/Concrete Music 8/sounds/"
  val SYNTH_DIR = "/Users/danielstahl/Documents/Projects/soundmining-modular/src/main/sc/synths"

  val CHAIR_DRAG_1 = "chair-drag-1"
  val CHAIR_DRAG_2 = "chair-drag-2"
  val APPLE_BITE = "apple-bite"
  val CHEST_HANDLE = "chest-handle"
  val CHEST_HIT = "chest-hit"
  val CHEST_LID = "chest-lid"
  val CHEST_SCRATCH_1 = "chest-scratch-1"
  val CHEST_SCRATCH_2 = "chest-scratch-2"
  val CLOCK_DOOR = "clock-door"
  val PEN_CLICK = "pen-click"
  val PEN_LID_HIT = "pen-lid-hit"
  val PEN_LID_RATTLE = "pen-lid-rattle"
  val PEN_LID_SCRATCH = "pen-lid-scratch"

  val soundPlays = SoundPlays(
    soundPlays = Map(
      APPLE_BITE -> SoundPlay(s"${SOUND_DIR}/Apple bite.flac", 0.024, 1.156),
      CHAIR_DRAG_1 -> SoundPlay(s"${SOUND_DIR}/Chair drag 1.flac", 0.0, 1.098),
      CHAIR_DRAG_2 -> SoundPlay(s"${SOUND_DIR}/Chair drag 2.flac", 0.196, 1.477),
      CHEST_HANDLE -> SoundPlay(s"${SOUND_DIR}/Chest handle.flac", 0.084, 1.142),
      CHEST_HIT -> SoundPlay(s"${SOUND_DIR}/Chest hit.flac", 0.162, 0.638),
      CHEST_LID -> SoundPlay(s"${SOUND_DIR}/Chest lid.flac", 0.000, 5.145),
      CHEST_SCRATCH_1 -> SoundPlay(s"${SOUND_DIR}/Chest scratch 1.flac", 0.000, 2.159),
      CHEST_SCRATCH_2 -> SoundPlay(s"${SOUND_DIR}/Chest scratch 2.flac", 0.171, 1.695),
      CLOCK_DOOR -> SoundPlay(s"${SOUND_DIR}/Clock door.flac", 0.400, 2.781),
      PEN_CLICK -> SoundPlay(s"${SOUND_DIR}/Pen click.flac", 0.019, 0.317),
      PEN_LID_HIT -> SoundPlay(s"${SOUND_DIR}/Pen lid hit.flac", 0.280, 0.955),
      PEN_LID_RATTLE -> SoundPlay(s"${SOUND_DIR}/Pen lid rattle.flac", 0.122, 2.110),
      PEN_LID_SCRATCH -> SoundPlay(s"${SOUND_DIR}/Pen lid scratch.flac", 0.058, 2.001),
    ),
    numberOfOutputBuses = 2)

  case class AudioNote() {
    val audioInstruments = mutable.Stack[AudioInstrument]()

    def saw(freq: Double, amp: ControlInstrument): AudioNote = {
      audioInstruments.push(sawOsc(amp, staticControl(freq)).addAction(TAIL_ACTION))
      this
    }

    def triangle(freq: Double, amp: ControlInstrument): AudioNote = {
      audioInstruments.push(triangleOsc(amp, staticControl(freq)).addAction(TAIL_ACTION))
      this
    }

    def pulse(freq: Double, amp: ControlInstrument): AudioNote = {
      audioInstruments.push(pulseOsc(amp, staticControl(freq)).addAction(TAIL_ACTION))
      this
    }

    def ring(modulatorFreq: Double): AudioNote = {
      audioInstruments.push(ringModulate(audioInstruments.pop(), staticControl(modulatorFreq)).addAction(TAIL_ACTION))
      this
    }

    def am(modulatorFreq: Double): AudioNote = {
      audioInstruments.push(amModulate(audioInstruments.pop(), staticControl(modulatorFreq)).addAction(TAIL_ACTION))
      this
    }

    def bandReject(freq: Double, rq: Double): AudioNote = {
      audioInstruments.push(bandRejectFilter(audioInstruments.pop(), staticControl(freq), staticControl(rq))
        .addAction(TAIL_ACTION))
      this
    }

    def bandPass(freq: Double, rq: Double): AudioNote = {
      audioInstruments.push(bandPassFilter(audioInstruments.pop(), staticControl(freq), staticControl(rq))
        .addAction(TAIL_ACTION))
      this
    }

    def highPass(freq: Double): AudioNote = {
      audioInstruments.push(highPassFilter(audioInstruments.pop(), staticControl(freq))
        .addAction(TAIL_ACTION))
      this
    }

    def lowPass(freq: Double): AudioNote = {
      audioInstruments.push(highPassFilter(audioInstruments.pop(), staticControl(freq))
        .addAction(TAIL_ACTION))
      this
    }

    def xfade(pan: ControlInstrument): AudioNote = {
      audioInstruments.push(ModularSynth.xfade(audioInstruments.pop(), audioInstruments.pop(), pan)
        .addAction(TAIL_ACTION))
      this
    }

    def pan(panPosition: Double): AudioNote =
      pan(staticControl(panPosition))

    def pan(startPan: Double, endPan: Double): AudioNote =
      pan(lineControl(startPan, endPan))

    def pan(panPosition: ControlInstrument): AudioNote = {
      audioInstruments.push(panning(audioInstruments.pop(), panPosition)
        .addAction(TAIL_ACTION)
        .withNrOfChannels(2))
      this
    }

    def play(start: Double, dur: Double, output: Int = 0): Unit = {
      val audioInstrument = audioInstruments.pop()
      audioInstrument.getOutputBus.staticBus(output)
      val graph = audioInstrument.buildGraph(start, dur, audioInstrument.graph(Seq()))
      client.send(client.newBundle(absoluteTimeToMillis(start), graph))
    }
  }

  /**
   * Pen lid hit
   * Frequencies with db
   * 5226 (-26), 3610 (-31), 2320 (-41)
   *
   * Pen lid rattle
   *
   * Frequencies with db
   * 3728.5 (-40), 9868.71 (-44), 5039 (-41), 2268.71 (-63) 954 (-68) 6368.84 (-79)
   *
   * Peak times
   * 0.192, 0.389
   * 0.553, 0.717
   * 0.720, 0.880
   * 0.882, 1.041
   * 1.042, 1.210
   * 1.212, 1.381
   * 1.382, 1.550
   * 1.551, 1.711
   * 1.714, 2046
   *
   *
   * Pen lid scratch
   *
   * Frequencies
   * 985, 2365, 5274
   *
   * 0.084, 0.144
   * 0.144, 0.396
   * 0.396, 0.574
   * 0.574, 0.885
   * 0.885, 1.273
   * 1.277, 1.500
   * 1.500, 1.711
   * 1.711, 2.030
   *
   */
  def metalBirdTheme(start: Double = 0, reset: Boolean = true): Unit = {
    if (reset) client.resetClock

    rattleMelody(start)
    rattleMelody(start + 1)

    scratchMelody(start + 2)

    rattleMelody(start + 3)
    rattleMelody(start + 4)

    scratchMelody(start + 5)

    rattleMelody(start + 6)

    hitMelody(start + 7)
    hitMelody(start + 9)

    scratchMelody(start + 10)

    hitMelody(start + 11)

    rattleMelody(start + 13)
    rattleMelody(start + 14)

    def hitMelody(start: Double): Unit = {
      soundPlays.mono(PEN_LID_HIT)
        .playMono(1.0, 1.0)
        .ring(3610)
        .highPass(3610)
        .splay(0.1, 0.1)
        .play(start, 0)

      soundPlays.mono(PEN_LID_HIT)
        .playMono(1.0, 2.0)
        .ring(5226)
        .highPass(3610)
        .splay(0.1, 0.5)
        .play(start, 0)

      soundPlays.mono(PEN_LID_HIT)
        .playMono(1.0, 2.0)
        .ring(2320)
        .highPass(2320)
        .splay(0.1, -0.1)
        .play(start + 0.2, 0)

      soundPlays.mono(PEN_LID_HIT)
        .playMono(1.0, 1.0)
        .ring(5226)
        .highPass(5226)
        .splay(0.1, -0.5)
        .play(start + 0.2, 0)

      soundPlays.mono(PEN_LID_HIT)
        .playMono(1.0, 1.0)
        .ring(3610)
        .highPass(3610)
        .splay(0.1, 0.1)
        .play(start + 0.5, 0)

      soundPlays.mono(PEN_LID_HIT)
        .playMono(1.0, 2.0)
        .ring(5226)
        .highPass(5226)
        .splay(0.1, 0.5)
        .play(start + 0.5, 0)

      soundPlays.mono(PEN_LID_HIT)
        .playMono(1.0, 2.0)
        .ring(2320)
        .highPass(2320)
        .splay(0.1, -0.1)
        .play(start + 0.7, 0)

      soundPlays.mono(PEN_LID_HIT)
        .playMono(1.0, 1.0)
        .ring(5226)
        .highPass(5226)
        .splay(0.1, -0.5)
        .play(start + 0.7, 0)
    }

    def rattleMelody(start: Double): Unit = {
      soundPlays.mono(PEN_LID_RATTLE)
        .playMono(0.192, 0.389, 1.0, 2.0)
        .ring(3728.5)
        .highPass(3728.5)
        .splay(0.1, -0.1)
        .play(start, 0)

      soundPlays.mono(PEN_LID_RATTLE)
        .playMono(0.192, 0.389, 1.0, 1.0)
        .ring(9868.71)
        .highPass(9868.71)
        .splay(0.1, -0.8)
        .play(start, 0)

      soundPlays.mono(PEN_LID_RATTLE)
        .playMono(0.720, 0.880, 1.0, 1.0)
        .ring(5039)
        .highPass(5039)
        .splay(0.1, 0.1)
        .play(start + 0.2, 0)

      soundPlays.mono(PEN_LID_RATTLE)
        .playMono(0.720, 0.880, 1.0, 2.0)
        .ring(6368.84)
        .highPass(6368.84)
        .splay(0.1, 0.8)
        .play(start + 0.2, 0)
    }

    def scratchMelody(start: Double): Unit = {
      soundPlays.mono(PEN_LID_SCRATCH)
        .playMono(0.144, 0.885, 1.0, 1.0)
        .ring(985)
        .highPass(985)
        .splay(0.1, 0.2)
        .play(start, 0)

      soundPlays.mono(PEN_LID_SCRATCH)
        .playMono(0.144, 0.885, 1.0, 2.0)
        .ring(5274)
        .highPass(5274)
        .splay(0.1, 0.7)
        .play(start, 0)

      soundPlays.mono(PEN_LID_SCRATCH)
        .playMono(1.277, 1.711, 1.0, 2.0)
        .ring(985)
        .highPass(985)
        .splay(0.1, -0.2)
        .play(start + 0.2, 0)

      soundPlays.mono(PEN_LID_SCRATCH)
        .playMono(1.277, 1.711, 1.0, 1.0)
        .ring(5274)
        .highPass(5274)
        .splay(0.1, -0.7)
        .play(start + 0.2, 0)
    }
  }

  /**
   * Chest handle, chest scratch 1 and chest scratch 2
   *
   * Chest handle
   * Frequencies with db
   * 144.793 (-23)
   * 241.599 (-27)
   * 401.936 (-41)
   * 627.114 (-39)
   * 706.733 (-35)
   *
   * Peak times
   * 0.135 0.334
   * 0.334 0.537
   * 0.734 1.090
   *
   * Chest scratch 1
   * Frequencies with db
   * 73.7946 (-37)
   * 141.959 (-36)
   * 205.203 (-38)
   * 302.337 (-40)
   * 850.504 (-34)
   * 1574.37 (-41)
   * 1737.36 (-39)
   *
   * Peak times
   * 0.003 0.484
   * 0.484 1.025
   * 1.025 1.292
   * 1.292 1.431
   * 1.431 1.846
   * 1.846 2.156
   *
   * Chest scratch 2
   * Frequencies with db
   * 55.1678 (-33)
   * 136.22 (-26)
   * 204.025 (-28)
   * 307.255 (-41)
   *
   * Peak times
   * 0.202 0.715
   * 0.715 1.238
   * 1.238 1.678
   *
  * */
  def woodBirdTheme(start: Double = 0, reset: Boolean = true): Unit = {
    if (reset) client.resetClock

    chestHandleMelody(start)
    chestHandleMelody(1)
    chestScratch1Melody(2)
    chestHandleMelody(4)
    chestScratch2Melody(6.5)
    chestScratch2Melody(5.5)
    chestScratch1Melody(7.5)
    chestHandleMelody(8.5)
    chestHandleMelody(9.5)

    def chestScratch2Melody(start: Double): Unit = {
      soundPlays.mono(CHEST_SCRATCH_2)
        .playMono(1.238, 1.678, 1.0, 1.0)
        .ring(204.025)
        .highPass(204.025)
        .splay(0.1, 0.3)
        .play(start, 0)

      soundPlays.mono(CHEST_SCRATCH_2)
        .playMono(1.238, 1.678, 1.0, 2.0)
        .ring(136.22)
        .lowPass(136.22)
        .splay(0.1, -0.5)
        .play(start, 0)

      soundPlays.mono(CHEST_SCRATCH_2)
        .playMono(0.202, 0.715, 1.0, 2.0)
        .ring(204.025)
        .highPass(204.025)
        .splay(0.1, -0.3)
        .play(start + 0.2, 0)

      soundPlays.mono(CHEST_SCRATCH_2)
        .playMono(0.202, 0.715, 1.0, 1.0)
        .ring(55.1678)
        .lowPass(55.1678)
        .splay(0.1, 0.7)
        .play(start + 0.2, 0)
    }

    def chestScratch1Melody(start: Double): Unit = {
      soundPlays.mono(CHEST_SCRATCH_1)
        .playMono(1.025, 1.292, 1.0, 2.0)
        .ring(205.203)
        .highPass(205.203)
        .splay(0.1, -0.4)
        .play(start, 0)

      soundPlays.mono(CHEST_SCRATCH_1)
        .playMono(1.025, 1.292, 1.0, 1.0)
        .ring(73.7946)
        .lowPass(73.7946)
        .splay(0.1, 0.1)
        .play(start, 0)

      soundPlays.mono(CHEST_SCRATCH_1)
        .playMono(1.003, 0.484, 1.0, 1.0)
        .ring(205.203)
        .highPass(205.203)
        .splay(0.1, 0.4)
        .play(start + 0.2, 0)

      soundPlays.mono(CHEST_SCRATCH_1)
        .playMono(1.003, 0.484, 1.0, 2.0)
        .ring(141.959)
        .lowPass(141.959)
        .splay(0.1, -0.1)
        .play(start + 0.2, 0)
    }

    def chestHandleMelody(start: Double): Unit = {
      soundPlays.mono(CHEST_HANDLE)
        .playMono(0.135, 0.334, 1.0, 1.0)
        .ring(144.793)
        .highPass(144.793)
        .splay(0.1, -0.2)
        .play(start, 0)

      soundPlays.mono(CHEST_HANDLE)
        .playMono(0.135, 0.334, 1.0, 2.0)
        .ring(627.114)
        .lowPass(627.114)
        .splay(0.1, -0.7)
        .play(start, 0)

      soundPlays.mono(CHEST_HANDLE)
        .playMono(0.135, 0.334, 1.0, 2.0)
        .ring(144.793)
        .highPass(144.793)
        .splay(0.1, 0.2)
        .play(start + 0.13, 0)

      soundPlays.mono(CHEST_HANDLE)
        .playMono(0.135, 0.334, 1.0, 1.0)
        .ring(706.733)
        .lowPass(706.733)
        .splay(0.1, 0.7)
        .play(start + 0.13, 0)
    }

  }

  def ringModulation(freq1: Double, freq2: Double): (Double, Double) =
    (freq1 - freq2, freq1 + freq2)

  def woodEnvironmentTheme(start: Double = 0, reset: Boolean = true): Unit = {
    if(reset) client.resetClock

    playEnv1(0)
    playEnv1(9)
    playEnv1(18)

    playEnv2(13)

    def playEnv2(start: Double): Unit = {
      AudioNote()
        .triangle(141.959, relativePercControl(0.001, 1, 0.5, Left(0)))
        .am(73.7946)
        .lowPass(205.203)
        .pan(-0.5, 0.5)
        .play(start, 13)

      AudioNote()
        .triangle(73.7946, relativePercControl(0.001, 1, 0.5, Left(0)))
        .ring(141.959)
        .highPass(205.203)
        .pan(0.2, -0.2)
        .play(start, 13)
    }

    def playEnv1(start: Double): Unit = {
      AudioNote()
        .triangle(144.793, relativeThreeBlockcontrol(0.001, 0.4, 1, 1, 0.3, 0.001, Left(0)))
        .ring(241.599)
        .lowPass(706.733)
        .triangle(241.599, relativeThreeBlockcontrol(0.001, 0.4, 1, 1, 0.3, 0.001, Left(0)))
        .am(144.793)
        .highPass(706.733)
        .xfade(lineControl(-1, 1))
        .pan(0.2, -0.4)
        .play(start, 8)

      AudioNote()
        .triangle(241.599, relativeThreeBlockcontrol(0.001, 0.4, 1, 1, 0.3, 0.001, Left(0)))
        .am(144.793)
        .lowPass(144.793)
        .triangle(144.793, relativeThreeBlockcontrol(0.001, 0.4, 1, 1, 0.3, 0.001, Left(0)))
        .ring(241.599)
        .highPass(401.936)
        .xfade(lineControl(-1, 1))
        .pan(-0.2, 0.4)
        .play(start + 5, 8)


/*
      AudioNote()
        .triangle(144.793, relativeThreeBlockcontrol(0.001, 0.4, 1, 1, 0.3, 0.001, Left(0)))
        .ring(241.599)
        .highPass(706.733)
        .pan(0.2, 0.7)
        .play(start, 8)


      AudioNote()
        .triangle(401.936, relativeThreeBlockcontrol(0.001, 0.4, 1, 1, 0.3, 0.001, Left(0)))
        .am(627.114)
        .highPass(241.599)
        .pan(-0.9, -0.4)
        .play(start + 5, 8)

      AudioNote()
        .triangle(401.936, relativeThreeBlockcontrol(0.001, 0.4, 1, 1, 0.3, 0.001, Left(0)))
        .ring(627.114)
        .lowPass(241.599)
        .pan(0.9, 0.4)
        .play(start + 5, 8)*/
    }
  }

  def metalEnvironmentTheme(start: Double = 0, reset: Boolean = true): Unit = {
    if(reset) client.resetClock

    playEnv1(start)
    playEnv1(start + 6)
    playEnv1(start + 12)
    playEnv1(start + 18)
    playEnv1short(start + 24)

    playEnv2(15)

    def playEnv2(start: Double): Unit = {
      AudioNote()
        .saw(3728.5, relativePercControl(0.001, 1, 0.5, Left(0)))
        .am(5039)
        .lowPass(3728.5)
        .pan(0.6, 0.3)
        .play(start, 5)

      AudioNote()
        .saw(3728.5, relativePercControl(0.001, 1, 0.5, Left(0)))
        .ring(5039)
        .highPass(3728.5)
        .pan(-0.2, -0.7)
        .play(start, 5)

      AudioNote()
        .saw(954, relativePercControl(0.001, 1, 0.5, Left(0)))
        .am(2268.71)
        .lowPass(3728.5)
        .pan(-0.6, -0.3)
        .play(start + 2, 5)

      AudioNote()
        .saw(954, relativePercControl(0.001, 1, 0.5, Left(0)))
        .ring(2268.71)
        .highPass(3728.5)
        .pan(0.2, 0.7)
        .play(start + 2, 5)

    }

    def playEnv1(start: Double): Unit =
      AudioNote()
        .saw(5226, relativeThreeBlockcontrol(0.001, 0.3, 1, 1, 0.3, 0.001, Left(0)))
        .ring(3610)
        .saw(3610, relativeThreeBlockcontrol(0.001, 0.3, 1, 1, 0.3, 0.001, Left(0)))
        .am(5226)
        .xfade(lineControl(-1, 1))
        .pan(-0.5, 0.5)
        .play(start, 8)

    def playEnv1short(start: Double): Unit =
      AudioNote()
        .saw(5226, relativeThreeBlockcontrol(0.001, 0.2, 1, 1, 0.1, 0.001, Left(0)))
        .ring(3610)
        .saw(3610, relativeThreeBlockcontrol(0.001, 0.2, 1, 1, 0.1, 0.001, Left(0)))
        .am(5226)
        .xfade(lineControl(-1, 1))
        .pan(-0.5, 0.5)
        .play(start, 5)



    //playSawRing(0, 5226, 3610, (0.2, 0.6), ring => ring, 13, -0.5)
    //playSawRing(0, 3610, 5226, (0.6, 0.1), ring => ring, 13, 0.5)
    //playSawAm(8, 5226, 3610, (0.2, 0.5), ring => ring, 13, 0.7)
    //playSawAm(8, 3610, 5226, (0.5, 0.2), ring => ring, 13, -0.7)
    //playSawLongRing(0, 5226, 3610, ring => bandRejectFilter(ring, staticControl(5226), staticControl(3)).addAction(TAIL_ACTION), 0.66, 13, 0.5)
    //playSawLongRing(5, 2320, 3610, 0.66, 13, 0.5)
    //playSawLongRing(8, 5226, 2320, 0.5, 13, 0)


    def playSawRing(start: Double, freq1: Double, freq2: Double, ad: (Double, Double), filterFunc: AudioInstrument => AudioInstrument, dur: Double, pan: Double): Unit = {

      val pulse = sawOsc(relativeThreeBlockcontrol(0.001, ad._1, 1, 1, ad._2, 0.001, Left(0)),
        staticControl(freq1))
        .addAction(TAIL_ACTION)

      val ring = ringModulate(pulse, staticControl(freq2))
        .addAction(TAIL_ACTION)

      val filter = filterFunc(ring)

      val splay = ModularSynth.panning(filter, staticControl(pan))
        .addAction(TAIL_ACTION)
        .withNrOfChannels(2)

      splay.getOutputBus.staticBus(0)
      val graph = splay.buildGraph(start, dur, splay.graph(Seq()))
      client.send(client.newBundle(absoluteTimeToMillis(start), graph))
    }

    def playSawAm(start: Double, freq1: Double, freq2: Double, ad: (Double, Double), filterFunc: AudioInstrument => AudioInstrument, dur: Double, pan: Double): Unit = {

      val pulse = sawOsc(relativeThreeBlockcontrol(0.001, ad._1, 1, 1, ad._2, 0.001, Left(0)),
        staticControl(freq1))
        .addAction(TAIL_ACTION)

      val ring = amModulate(pulse, staticControl(freq2))
        .addAction(TAIL_ACTION)

      val filter = filterFunc(ring)

      val splay = ModularSynth.panning(filter, staticControl(pan))
        .addAction(TAIL_ACTION)
        .withNrOfChannels(2)

      splay.getOutputBus.staticBus(0)
      val graph = splay.buildGraph(start, dur, splay.graph(Seq()))
      client.send(client.newBundle(absoluteTimeToMillis(start), graph))
    }
  }

  def init(): Unit = {
    println("Starting up SuperCollider client")
    client.start
    Instrument.setupNodes(client)
    client.send(loadDir(SYNTH_DIR))
    soundPlays.init
  }

  def stop(): Unit = {
    println("Stopping SuperCollider client")
    client.stop
  }
}
