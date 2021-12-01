package net.soundmining

import net.soundmining.modular.ModularInstrument.{AudioInstrument, ControlInstrument}
import net.soundmining.modular.ModularSynth
import net.soundmining.modular.ModularSynth.{amModulate, bandPassFilter, bandRejectFilter, highPassFilter, lineControl, panning, pulseOsc, relativePercControl, relativeThreeBlockcontrol, ringModulate, sawOsc, staticControl, triangleOsc}
import net.soundmining.sound.{SoundPlay, SoundPlays}
import net.soundmining.synth.Instrument.TAIL_ACTION
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

  object MetalBirdTheme {
    val one: Double = 1.550 - 0.553

    def playShort(start: Double = 0, reset: Boolean = true): Unit = {
      if (reset) client.resetClock

      val times = Melody.absolute(start, Seq(
        one, one, one))

      rattleMelody(times.head)
      rattleMelody(times(1))

      scratchMelody(times(2))
    }

    def playShort2(start: Double = 0, reset: Boolean = true): Unit = {
      if (reset) client.resetClock

      val times = Melody.absolute(start, Seq(
        one, one, one, one, one * 2, one))

      rattleMelody(times.head)
      rattleMelody(times(1))

      scratchMelody(times(2))

      rattleMelody(times(3))

      hitMelody(times(4))
      hitMelody(times(5))
    }

    def playLong(start: Double = 0, reset: Boolean = true): Unit = {
      if (reset) client.resetClock

      val times = Melody.absolute(start, Seq(
        one, one, one, one, one, one, one, one * 2,
        one, one, one * 2, one, one))

      rattleMelody(times.head)
      rattleMelody(times(1))

      scratchMelody(times(2))

      rattleMelody(times(3))
      rattleMelody(times(4))

      scratchMelody(times(5))

      rattleMelody(times(6))

      hitMelody(times(7))
      hitMelody(times(8))

      scratchMelody(times(9))

      hitMelody(times(10))

      rattleMelody(times(11))
      rattleMelody(times(12))
    }

    def hitMelody(start: Double): Unit = {
      val short = 0.389 - 0.192
      val middle = 1.550 - 1.042

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
        .play(start + short, 0)

      soundPlays.mono(PEN_LID_HIT)
        .playMono(1.0, 1.0)
        .ring(5226)
        .highPass(5226)
        .splay(0.1, -0.5)
        .play(start + short, 0)

      soundPlays.mono(PEN_LID_HIT)
        .playMono(1.0, 1.0)
        .ring(3610)
        .highPass(3610)
        .splay(0.1, 0.1)
        .play(start + middle, 0)

      soundPlays.mono(PEN_LID_HIT)
        .playMono(1.0, 2.0)
        .ring(5226)
        .highPass(5226)
        .splay(0.1, 0.5)
        .play(start + middle, 0)

      soundPlays.mono(PEN_LID_HIT)
        .playMono(1.0, 2.0)
        .ring(2320)
        .highPass(2320)
        .splay(0.1, -0.1)
        .play(start + middle + short, 0)

      soundPlays.mono(PEN_LID_HIT)
        .playMono(1.0, 1.0)
        .ring(5226)
        .highPass(5226)
        .splay(0.1, -0.5)
        .play(start + middle + short, 0)
    }

    def rattleMelody(start: Double): Unit = {
      val shortTime = 0.389 - 0.192

      soundPlays.mono(PEN_LID_RATTLE)
        .playMono(0.192, 0.389, 1.0, 3.0)
        .ring(3728.5)
        .highPass(3728.5)
        .splay(0.1, -0.1)
        .play(start, 0)

      soundPlays.mono(PEN_LID_RATTLE)
        .playMono(0.192, 0.389, 1.0, 2.0)
        .ring(9868.71)
        .highPass(9868.71)
        .splay(0.1, -0.8)
        .play(start, 0)

      soundPlays.mono(PEN_LID_RATTLE)
        .playMono(0.192, 0.389, 1.0, 1.0)
        .ring(2268.71)
        .lowPass(9868.71)
        .splay(0.1, -0.4)
        .play(start, 0)

      soundPlays.mono(PEN_LID_RATTLE)
        .playMono(0.720, 0.880, 1.0, 2.0)
        .ring(5039)
        .highPass(5039)
        .splay(0.1, 0.1)
        .play(start + shortTime, 0)

      soundPlays.mono(PEN_LID_RATTLE)
        .playMono(0.720, 0.880, 1.0, 3.0)
        .ring(6368.84)
        .highPass(6368.84)
        .splay(0.1, 0.8)
        .play(start + shortTime, 0)

      soundPlays.mono(PEN_LID_RATTLE)
        .playMono(0.720, 0.880, 1.0, 1.0)
        .ring(6368.84)
        .lowPass(9868.71)
        .splay(0.1, 0.4)
        .play(start + shortTime, 0)
    }

    def scratchMelody(start: Double): Unit = {
      val shortTime = 0.574 - 0.396
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
        .playMono(0.144, 0.885, 1.0, 1.0)
        .ring(2365)
        .bandPass(985, 2)
        .splay(0.1, 0.4)
        .play(start, 0)

      soundPlays.mono(PEN_LID_SCRATCH)
        .playMono(1.277, 1.711, 1.0, 2.0)
        .ring(985)
        .highPass(985)
        .splay(0.1, -0.2)
        .play(start + shortTime, 0)

      soundPlays.mono(PEN_LID_SCRATCH)
        .playMono(1.277, 1.711, 1.0, 1.0)
        .ring(5274)
        .highPass(5274)
        .splay(0.1, -0.7)
        .play(start + shortTime, 0)

      soundPlays.mono(PEN_LID_SCRATCH)
        .playMono(1.277, 1.711, 1.0, 1.0)
        .ring(2365)
        .bandPass(985, 2)
        .splay(0.1, -0.4)
        .play(start + shortTime, 0)
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

  object WoodBirdTheme {
    val short = 0.484 - 0.003

    def playShort1(start: Double = 0, reset: Boolean = true): Unit = {
      if (reset) client.resetClock

      val times = Melody.absolute(start,
        Seq(2, 2)
          .map(_ * short))
      chestHandleMelody(times.head)
      chestHandleMelody(times(1))
    }

    def playShort2(start: Double = 0, reset: Boolean = true): Unit = {
      if (reset) client.resetClock

      val times = Melody.absolute(start,
        Seq(2, 2, 4, 3)
          .map(_ * short))
      chestHandleMelody(times.head)
      chestHandleMelody(times(1))
      chestScratch1Melody(times(2))
      chestHandleMelody(times(3))
    }

    def playShort3(start: Double = 0, reset: Boolean = true): Unit = {
      if (reset) client.resetClock

      val times = Melody.absolute(start,
        Seq(2, 2, 4, 3, 2, 2, 2)
          .map(_ * short))
      chestHandleMelody(times.head)
      chestHandleMelody(times(1))
      chestScratch1Melody(times(2))
      chestHandleMelody(times(3))
      chestScratch2Melody(times(4))
      chestScratch2Melody(times(5))
      chestScratch1Melody(times(6))
    }

    def playLong(start: Double = 0, reset: Boolean = true): Unit = {
      if (reset) client.resetClock

      val times = Melody.absolute(start,
        Seq(2, 2, 4, 3, 2, 2, 2, 2, 2)
          .map(_ * short))

      chestHandleMelody(times.head)
      chestHandleMelody(times(1))
      chestScratch1Melody(times(2))
      chestHandleMelody(times(3))
      chestScratch2Melody(times(4))
      chestScratch2Melody(times(5))
      chestScratch1Melody(times(6))
      chestHandleMelody(times(7))
      chestHandleMelody(times(8))
    }

    def chestScratch2Melody(start: Double): Unit = {
      val short = 0.2
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
        .play(start + short, 0)

      soundPlays.mono(CHEST_SCRATCH_2)
        .playMono(0.202, 0.715, 1.0, 1.0)
        .ring(55.1678)
        .lowPass(55.1678)
        .splay(0.1, 0.7)
        .play(start + short, 0)
    }

    def chestScratch1Melody(start: Double): Unit = {
      val short = 0.2
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
        .play(start + short, 0)

      soundPlays.mono(CHEST_SCRATCH_1)
        .playMono(1.003, 0.484, 1.0, 2.0)
        .ring(141.959)
        .lowPass(141.959)
        .splay(0.1, -0.1)
        .play(start + short, 0)
    }

    def chestHandleMelody(start: Double): Unit = {
      val short = 0.13
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
        .play(start + short, 0)

      soundPlays.mono(CHEST_HANDLE)
        .playMono(0.135, 0.334, 1.0, 1.0)
        .ring(706.733)
        .lowPass(706.733)
        .splay(0.1, 0.7)
        .play(start + short, 0)
    }


  }

  def ringModulation(freq1: Double, freq2: Double): (Double, Double) =
    (freq1 - freq2, freq1 + freq2)

  object WoodEnvironmentTheme {

    val one = 1.025 - 0.003

    def playLong(start: Double = 0, reset: Boolean = true): Unit = {
      if(reset) client.resetClock

      val times = Melody.absolute(start, Seq(9, 9, 9).map(_ * one))
      val startTwo = start + (13 * one)

      playEnv1(times.head)
      playEnv1(times(1))
      playEnv1(times(2))

      playEnv2(startTwo)
    }

    def playShort1(start: Double = 0, reset: Boolean = true): Unit = {
      if(reset) client.resetClock

      playEnv1(start)
    }

    def playShort2(start: Double = 0, reset: Boolean = true): Unit = {
      if(reset) client.resetClock

      val times = Melody.absolute(start, Seq(9, 9).map(_ * one))

      playEnv1(times.head)
      playEnv1(times(1))
    }

    def playEnv2(start: Double): Unit = {
      AudioNote()
        .triangle(141.959, relativePercControl(0.001, 1, 0.5, Left(0)))
        .am(73.7946)
        .lowPass(205.203)
        .pan(-0.5, 0.5)
        .play(start, 13 * one)

      AudioNote()
        .triangle(73.7946, relativePercControl(0.001, 1, 0.5, Left(0)))
        .ring(141.959)
        .highPass(205.203)
        .pan(0.2, -0.2)
        .play(start, 13 * one)
    }

    def playEnv2short(start: Double): Unit = {
      AudioNote()
        .triangle(141.959, relativePercControl(0.001, 1, 0.5, Left(0)))
        .am(73.7946)
        .lowPass(205.203)
        .pan(-0.5, 0.5)
        .play(start, 8 * one)

      AudioNote()
        .triangle(73.7946, relativePercControl(0.001, 1, 0.5, Left(0)))
        .ring(141.959)
        .highPass(205.203)
        .pan(0.2, -0.2)
        .play(start, 8 * one)
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
        .play(start, 8 * one)

      AudioNote()
        .triangle(241.599, relativeThreeBlockcontrol(0.001, 0.4, 1, 1, 0.3, 0.001, Left(0)))
        .am(144.793)
        .lowPass(144.793)
        .triangle(144.793, relativeThreeBlockcontrol(0.001, 0.4, 1, 1, 0.3, 0.001, Left(0)))
        .ring(241.599)
        .highPass(401.936)
        .xfade(lineControl(-1, 1))
        .pan(-0.2, 0.4)
        .play(start + (5 * one), 8 * one)
    }
  }

  object MetalEnvironmentTheme {
    val one: Double = 1.550 - 0.553

    def playLong(start: Double = 0, reset: Boolean = true): Unit = {
      if(reset) client.resetClock

      val times = Melody.absolute(start, Seq(
        one * 6, one * 6, one * 6, one * 6, one * 6))

      val startTwo = one * 15

      playEnv1(times.head)
      playEnv1(times(1))
      playEnv1(times(2))
      playEnv1(times(3))
      playEnv1short(times(4))

      playEnv2(startTwo)
    }

    def playShort1(start: Double = 0, reset: Boolean = true): Unit = {
      if(reset) client.resetClock

      playEnv1(start)
    }

    def playShort2(start: Double = 0, reset: Boolean = true): Unit = {
      if(reset) client.resetClock
      val start2 = start + (one * 6)

      playEnv1(start)
      playEnv1(start2)
    }

    def playShort3(start: Double = 0, reset: Boolean = true): Unit = {
      if(reset) client.resetClock

      val times = Melody.absolute(start, Seq(
        one * 6, one * 6, one * 6))

      val startTwo = start + (one * 9)
      playEnv1(times.head)
      playEnv1(times(1))
      playEnv1short(times(2))

      playEnv2(startTwo)
    }

    def playEnv2(start: Double): Unit = {
      AudioNote()
        .saw(3728.5, relativePercControl(0.001, 1, 0.5, Left(0)))
        .am(5039)
        .lowPass(3728.5)
        .pan(0.6, 0.3)
        .play(start, one * 5)

      AudioNote()
        .saw(3728.5, relativePercControl(0.001, 1, 0.5, Left(0)))
        .ring(5039)
        .highPass(3728.5)
        .pan(-0.2, -0.7)
        .play(start, one * 5)

      AudioNote()
        .saw(954, relativePercControl(0.001, 1, 0.5, Left(0)))
        .am(2268.71)
        .lowPass(3728.5)
        .pan(-0.6, -0.3)
        .play(start + (one * 2), one * 5)

      AudioNote()
        .saw(954, relativePercControl(0.001, 1, 0.5, Left(0)))
        .ring(2268.71)
        .highPass(3728.5)
        .pan(0.2, 0.7)
        .play(start + (one * 2), one * 5)
    }

    def playEnv1(start: Double): Unit =
      AudioNote()
        .saw(5226, relativeThreeBlockcontrol(0.001, 0.3, 1, 1, 0.3, 0.001, Left(0)))
        .ring(3610)
        .saw(3610, relativeThreeBlockcontrol(0.001, 0.3, 1, 1, 0.3, 0.001, Left(0)))
        .am(5226)
        .xfade(lineControl(-1, 1))
        .pan(-0.5, 0.5)
        .play(start, one * 8)

    def playEnv1short(start: Double): Unit =
      AudioNote()
        .saw(5226, relativeThreeBlockcontrol(0.001, 0.2, 1, 1, 0.1, 0.001, Left(0)))
        .ring(3610)
        .saw(3610, relativeThreeBlockcontrol(0.001, 0.2, 1, 1, 0.1, 0.001, Left(0)))
        .am(5226)
        .xfade(lineControl(-1, 1))
        .pan(-0.5, 0.5)
        .play(start, one * 5)
  }

  def playMetalBirdFirstPart(start: Double = 0, reset: Boolean = true): Unit = {
    if(reset) client.resetClock

    val start2 = start + MetalEnvironmentTheme.one * 7

    MetalEnvironmentTheme.playShort1(start, false)
    MetalBirdTheme.playShort(start2, false)

    val start3 = start2 + (MetalBirdTheme.one * 5)
    MetalEnvironmentTheme.playShort2(start3, false)

    val start4 = start3 + (MetalEnvironmentTheme.one * 13)
    MetalBirdTheme.playShort2(start4, false)

    val start5 = start4 + (MetalBirdTheme.one * 9)
    MetalEnvironmentTheme.playShort3(start5, false)
  }

  def playWoodBirdFirstPart(start: Double = 0, reset: Boolean = true): Unit = {
    if(reset) client.resetClock

    WoodBirdTheme.playShort1(start, false)
    val start2 = start + (WoodBirdTheme.short * 3)
    WoodEnvironmentTheme.playShort1(start2, false)
    val start3 = start2 + (WoodEnvironmentTheme.one * 12)
    WoodBirdTheme.playShort2(start3, false)
    val start4 = start3 + (WoodBirdTheme.short * 9)
    WoodEnvironmentTheme.playShort2(start4, false)
    val start5 = start4 + (WoodEnvironmentTheme.one * 21)
    WoodBirdTheme.playShort3(start5, false)
    val start6 = start5 + (WoodBirdTheme.short * 16)
    WoodEnvironmentTheme.playLong(start6, false)
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
