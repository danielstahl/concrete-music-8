package net.soundmining

import net.soundmining.modular.ModularInstrument.{AudioInstrument, ControlInstrument, StaticAudioBusInstrument}
import net.soundmining.modular.ModularSynth
import net.soundmining.modular.ModularSynth.{bandRejectFilter, controlMultiply, highPassFilter, lineControl, lowPassFilter, monoDelay, pulseOsc, relativePercControl, ringModulate, sawOsc, sineControl, sineOsc, staticAudioBus, staticControl, triangleOsc}
import net.soundmining.sound.{SoundPlay, SoundPlays}
import net.soundmining.synth.Instrument.{EFFECT, TAIL_ACTION}
import net.soundmining.synth.SuperColliderClient.loadDir
import net.soundmining.synth.Utils.absoluteTimeToMillis
import net.soundmining.synth.{Instrument, SuperColliderClient}

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

  /*
  * Combinations of sound tell little stories. Reflections in long saw rings.
  * Several stories. Each one evolve.
  * */

  /**
   * Pen lid hit
   * Frequencies with db
   * 5226 (-26), 3610 (-31), 2320 (-41)
   *
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
  def theme1(start: Double = 0, reset: Boolean = true): Unit = {
    if(reset) client.resetClock

    soundPlays.mono(PEN_LID_HIT)
      .playMono(1.0, 1.0)
      .ring(3610)
      .splay(0.1, 0.2)
      .play(start, 0)

    soundPlays.mono(PEN_LID_HIT)
      .playMono(1.0, 1.0)
      .ring(5226)
      .splay(0.1, -0.2)
      .play(start + 0.3, 0)


  }

  def theme2(start: Double = 0, reset: Boolean = true): Unit = {
    if(reset) client.resetClock

    soundPlays.mono(PEN_LID_RATTLE)
      .playMono(0.192, 0.880, 1.0, 1.0)
      .ring(4990)
      .highPass(4990)
      .splay(0.1, -0.9)
      .play(start, 0)

    soundPlays.mono(PEN_LID_RATTLE)
      .playMono(1.212, 2.046, 1.0, 1.0)
      .ring(6278)
      .lowPass(3728.5)
      .splay(0.1, 0.9)
      .play(start + 0.362, 0)
  }

  def theme3(start: Double = 0, reset: Boolean = true): Unit = {
    if(reset) client.resetClock

    soundPlays.mono(PEN_LID_SCRATCH)
      .playMono(0.144, 0.574, 1.0, 1.0)
      .ring(985)
      .highPass(985)
      .splay(0.1, 0.2)
      .play(start, 0)

    soundPlays.mono(PEN_LID_SCRATCH)
      .playMono(0.396, 0.885, 1.0, 1.0)
      .ring(2365)
      .highPass(2365)
      .splay(0.1, -0.2)
      .play(start + 0.2, 0)
  }

  def theme4(start: Double = 0, reset: Boolean = true): Unit = {
    if (reset) client.resetClock

    soundPlays.mono(PEN_LID_HIT)
      .playMono(1.0, 1.0)
      .ring(3610)
      .highPass(3610)
      .splay(0.1, 0.2)
      .play(start, 0)

    soundPlays.mono(PEN_LID_SCRATCH)
      .playMono(0.144, 0.574, 1.0, 1.0)
      .ring(985)
      .highPass(985)
      .splay(0.1, 0)
      .play(start + 0.3, 0)

    soundPlays.mono(PEN_LID_RATTLE)
      .playMono(0.192, 0.880, 1.0, 1.0)
      .ring(3728.5)
      .highPass(3728.5)
      .splay(0.1, -0.2)
      .play(start + 0.5, 0)

    soundPlays.mono(PEN_LID_HIT)
      .playMono(1.0, 1.0)
      .ring(5226)
      .highPass(5226)
      .splay(0.1, 0.2)
      .play(start + 1.2, 0)
  }

  def theme5(start: Double = 0, reset: Boolean = true): Unit = {
    if (reset) client.resetClock

    soundPlays.mono(PEN_LID_RATTLE)
      .playMono(0.192, 0.717, 1.0, 1.0)
      .ring(2268.71)
      .lowPass(2268.71)
      .splay(0.1, -0.5)
      .play(start, 0)

    soundPlays.mono(PEN_LID_HIT)
      .playMono(1.0, 1.0)
      .ring(2320)
      .lowPass(2320)
      .splay(0.1, 0.5)
      .play(start + 0.5, 0)

    soundPlays.mono(PEN_LID_SCRATCH)
      .playMono(0.084, 0.574, 1.0, 1.0)
      .ring(5274)
      .highPass(5274)
      .splay(0.1, 0)
      .play(start + 0.8, 0)

    soundPlays.mono(PEN_LID_HIT)
      .playMono(1.0, 1.0)
      .ring(5226)
      .highPass(5226)
      .splay(0.1, 0.5)
      .play(start + 1.4, 0)
  }

  def testDoubleDelay(start: Double = 0, reset: Boolean = true): Unit = {
    if(reset) client.resetClock

    val delayAudioBus = staticAudioBus()
    val shortDelay = monoDelay(delayAudioBus, staticControl(1), 0.1, 1)
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)

    val longDelay = monoDelay(shortDelay, staticControl(1), 1, 7)
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)

    val ring = ringModulate(longDelay, staticControl(3610))
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)

    val filter = highPassFilter(ring, staticControl(3610))
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)

    val splay = ModularSynth.splay(filter, staticControl(0.1), centerBus = staticControl(0))
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)
      .withNrOfChannels(2)

    splay.getOutputBus.staticBus(0)
    val graph = splay.buildGraph(0, 30, splay.graph(Seq()))
    client.send(client.newBundle(0, graph))


    soundPlays.mono(PEN_LID_HIT)
      .playMono(1.0, 1.0)
      .play(start, delayAudioBus)


  }

  /*
  * Idea. Mechanical birds. Repeating. Short. One metal, one wood
  * */

  def theme6(start: Double = 0, reset: Boolean = true): Unit = {
    if(reset) client.resetClock

    def createDelayHighpass(delay: Double, decay: Double, pan: Double, ringFreq: Double): StaticAudioBusInstrument = {
      val filter = (input: AudioInstrument, freq: Double) =>
        highPassFilter(input, staticControl(freq))
          .addAction(TAIL_ACTION)
          .nodeId(EFFECT)

      createDelay(delay, decay, pan, ringFreq, filter)
    }

    def createDelayLowpass(delay: Double, decay: Double, pan: Double, ringFreq: Double): StaticAudioBusInstrument = {
      val filter = (input: AudioInstrument, freq: Double) =>
        lowPassFilter(input, staticControl(freq))
          .addAction(TAIL_ACTION)
          .nodeId(EFFECT)

      createDelay(delay, decay, pan, ringFreq, filter)
    }

    def createDelay(delayTime: Double, decayTime: Double, pan: Double, ringFreq: Double, filterFunc: (AudioInstrument, Double) => AudioInstrument): StaticAudioBusInstrument = {
      val delayAudioBus = staticAudioBus()
      val delay = monoDelay(delayAudioBus, staticControl(1), delayTime, decayTime)
        .addAction(TAIL_ACTION)
        .nodeId(EFFECT)

      val ring = ringModulate(delay, staticControl(ringFreq))
        .addAction(TAIL_ACTION)
        .nodeId(EFFECT)

      val filter = filterFunc.apply(ring, ringFreq)
        .asInstanceOf[AudioInstrument]

      val splay = ModularSynth.splay(filter, staticControl(0.1), centerBus = staticControl(pan))
        .addAction(TAIL_ACTION)
        .nodeId(EFFECT)
        .withNrOfChannels(2)

      splay.getOutputBus.staticBus(0)
      val graph = splay.buildGraph(0, 15, splay.graph(Seq()))
      client.send(client.newBundle(0, graph))

      delayAudioBus
    }

    val delayAudioBus1 = createDelayLowpass(0.300, 10, 0.5, 3610)

    val delayAudioBus2 = createDelayHighpass(0.250, 10, -0.5, 5226)

    soundPlays.mono(PEN_LID_HIT)
      .playMono(1.0, 1.0)
      .play(start, delayAudioBus1)

    soundPlays.mono(PEN_LID_HIT)
      .playMono(1.0, 1.0)
      .play(start + 0.3, delayAudioBus2)
  }

  /*
  * Idea. Mechanical birds. Repeating. Short. One metal, one wood
  * */
  def theme7(start: Double = 0, reset: Boolean = true): Unit = {
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

      // 2 3 2
      soundPlays.mono(PEN_LID_HIT)
        .playMono(1.0, 1.0)
        .ring(3610)
        .highPass(3610)
        .splay(0.1, 0.1)
        .play(start, 0)

      soundPlays.mono(PEN_LID_HIT)
        .playMono(1.0, 1.0)
        .ring(2320)
        .highPass(2320)
        .splay(0.1, -0.1)
        .play(start + 0.2, 0)

      soundPlays.mono(PEN_LID_HIT)
        .playMono(1.0, 1.0)
        .ring(3610)
        .highPass(3610)
        .splay(0.1, 0.1)
        .play(start + 0.5, 0)

      soundPlays.mono(PEN_LID_HIT)
        .playMono(1.0, 1.0)
        .ring(2320)
        .highPass(2320)
        .splay(0.1, -0.1)
        .play(start + 0.7, 0)
    }

    def rattleMelody(start: Double): Unit = {
      soundPlays.mono(PEN_LID_RATTLE)
        .playMono(0.192, 0.389, 1.0, 1.0)
        .ring(3728.5)
        .highPass(3728.5)
        .splay(0.1, -0.1)
        .play(start, 0)

      soundPlays.mono(PEN_LID_RATTLE)
        .playMono(0.192, 0.389, 1.0, 1.0)
        .ring(5039)
        .highPass(5039)
        .splay(0.1, 0.1)
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
        .playMono(1.277, 1.711, 1.0, 1.0)
        .ring(985)
        .highPass(985)
        .splay(0.1, -0.2)
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


  def theme8(start: Double = 0, reset: Boolean = true): Unit = {
    if (reset) client.resetClock

    chestHandleMelody(start)
    chestHandleMelody(1)
    chestScratch1Melody(2)
    chestHandleMelody(4)
    chestScratch2Melody(5.5)
    chestScratch2Melody(6.5)
    chestScratch1Melody(7.5)
    chestHandleMelody(8.5)
    chestHandleMelody(9.5)

    def chestScratch2Melody(start: Double): Unit = {
      soundPlays.mono(CHEST_SCRATCH_2)
        .playMono(1.238, 1.678, 1.0, 1.0)
        .ring(204.025)
        .highPass(204.025)
        .splay(0.1, 0.2)
        .play(start, 0)

      soundPlays.mono(CHEST_SCRATCH_2)
        .playMono(0.202, 0.715, 1.0, 1.0)
        .ring(204.025)
        .highPass(204.025)
        .splay(0.1, -0.2)
        .play(start + 0.2, 0)
    }

    def chestScratch1Melody(start: Double): Unit = {
      soundPlays.mono(CHEST_SCRATCH_1)
        .playMono(1.025, 1.292, 1.0, 1.0)
        .ring(205.203)
        .highPass(205.203)
        .splay(0.1, -0.2)
        .play(start, 0)

      soundPlays.mono(CHEST_SCRATCH_1)
        .playMono(1.003, 0.484, 1.0, 1.0)
        .ring(205.203)
        .highPass(205.203)
        .splay(0.1, 0.2)
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
        .playMono(0.135, 0.334, 1.0, 1.0)
        .ring(144.793)
        .highPass(144.793)
        .splay(0.1, 0.2)
        .play(start + 0.13, 0)
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
