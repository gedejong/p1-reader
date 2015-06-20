package nl.edejong.p1.parser

import java.text.NumberFormat

import org.joda.time.{DateTimeZone, DateTime}
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import org.joda.time.tz.DateTimeZoneBuilder
import squants.{Quantity, UnitOfMeasure, Dimension}
import squants.electro.{Amperes, ElectricCurrent}
import squants.energy.{Energy, Power, KilowattHours, Kilowatts}
import squants.time.{Time, Seconds}

import scala.util.Try
import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.combinator.{RegexParsers, Parsers}
import scala.util.parsing.input.CharSequenceReader

trait BaseParsers extends RegexParsers {
  def baseValue: Parser[String] = """[^ \r\n!\)]+""".r
  def booleanValue: Parser[String] = """(1|0)""".r
  def hexadecimalValue(min: Int, max: Int): Parser[String] = s"""[0-9a-fA-F]{$min,$max}""".r
  def decimalValue(l: Int): Parser[String] = s"""[0-9]{$l}|[0-9.]{${l+1}}""".r
  def floatValue(): Parser[String] = s"""[0-9][0-9.]*""".r
  def decimalValue(): Parser[String] = """[0-9]+""".r
  def timestampValue: Parser[String] = """\d{12}(S|W)""".r
  def stringValue(length: Int): Parser[String] = s"""[^)]{$length}""".r
  def variableString(min: Int, max: Int): Parser[String] = s"""[^)\n]{$min,$max}""".r
  def nl: Parser[String] = "\n" | "\r\n"

  override val skipWhitespace = false
}

class P1Parser extends BaseParsers with DataMessageSyntax {

  def nullData: Parser[Unit] = ???

  def boolean: Parser[Boolean] = booleanValue ^^ { _ == "1" }

  def bitString(min: Int, max: Int): Parser[BigInt] =
    hexadecimalValue(min, max) ^^ { str ⇒
      val length = Integer.parseInt(str.head.toString, 16)
      BigInt.apply(str.tail, 16)
    }

  def doubleLong(length: Int, minPrecision: Int = 0, maxPrecision: Int = 0): Parser[Long] =
    decimalValue(length) ^^ {
      case str if str.length == length ⇒ str.toLong
    }

  def doubleLongUnsigned(length: Int, minPrecision: Int = 0, maxPrecision: Int = 0): Parser[Long] =
    doubleLong(length, minPrecision, maxPrecision)

  def floatingPoint(length: Int, minPrecision: Int, maxPrecision: Int): Parser[BigDecimal] =
    decimalValue(length) ^? {
      case str if Try(BigDecimal(str)).isSuccess ⇒ BigDecimal(str)
    }

  def floatingPoint(): Parser[BigDecimal] = floatValue() ^? {
    case str if Try(BigDecimal(str)).isSuccess ⇒ BigDecimal(str)
  }

  def octetString(min: Int, max: Int): Parser[Array[Byte]] =
    hexadecimalValue(min, max) ^^ { str ⇒
      val byteInts = for (byteStr ← str.grouped(2))
        yield Integer.parseInt(byteStr.mkString(""), 16).asInstanceOf[Byte]
      byteInts.toArray
    }

  def octetStringAsString(min: Int, max: Int): Parser[String] =
    octetString(min, max).map(new String(_))

  def visibleString(length: Int): Parser[String] = stringValue(length)

  def bcd: BigDecimal = ???

  def integer(length: Int): Parser[Int] = decimalValue(length).map(_.toInt)
  def long(length: Int, minPrecision: Int = 0, maxPrecision: Int = 0): Parser[Long] = ???
  def unsigned(length: Int, minPrecision: Int = 0, maxPrecision: Int = 0): Parser[Long] = ???
  def longUnsigned(length: Int, minPrecision: Int = 0, maxPrecision: Int = 0): Parser[Long] = ???
  def long64(length: Int, minPrecision: Int = 0, maxPrecision: Int = 0): Parser[BigInt] = ???
  def long64Unsigned(length: Int, minPrecision: Int = 0, maxPrecision: Int = 0): Parser[BigInt] = ???
  def enum(length: Int): Parser[Int] = ???
  def floating32(length: Int, minPrecision: Int, maxPrecision: Int): Parser[Float] = ???
  def floating64(length: Int, minPrecision: Int, maxPrecision: Int): Parser[Double] = ???

  private val datetimeFormat =
    DateTimeFormat
      .forPattern("YYMMddHHmmss")
      .withZone(DateTimeZone.forID("Europe/Amsterdam"))

  def timestamp: Parser[DateTime] = timestampValue ^^ { str ⇒
    // Todo: Need to figure out if the S/W (summer/winter time, has any
    // useful addition the calculation Joda DateTime already does.
    datetimeFormat.parseDateTime(str.take(12))
  }

  def decimals: Parser[Int] = decimalValue() ^^ { str ⇒ str.toInt }

  // """\\d+-\\d+:\\d+.\\d+.\\d+.\\d+"""
  def obisRef =
    decimals ~ "-" ~ decimals ~ ":" ~ decimals ~ "." ~ decimals ~ "." ~ decimals ^^ {
      case maj ~ "-" ~ min ~ ":" ~ a ~ "." ~ b ~ "." ~ c ⇒ (maj, min, a, b, c)
    }

  type ObisRef = (Int, Int, Int, Int, Int)
  type MBusObisRef = (Int, Int, Int, Int)

  implicit def obisRefMatch(id: ObisRef) = obisRef ^? {
    case parsedId if parsedId == id ⇒ parsedId
  }

  implicit def mbusRefMatch(id: MBusObisRef) = obisRef ^? {
    case (maj, min, a, b, c) if maj == id._1 && a == id._2 && b == id._3 && c == id._4 ⇒ min
  }

  def unitOfMeasure[T <: Quantity[T]](d: Dimension[T]): Parser[UnitOfMeasure[T]] =
    variableString(1, 10).map (str ⇒ (str, d.symbolToUnit(str))) ^? (
      { case (_, Some(unit)) ⇒ unit },
      { case (str, _) ⇒ s"""$str is not in set of allowed units [${d.units.map(_.symbol).mkString(", ")}]""" }
    )

  def cosemValue[T <: Quantity[T], N](valueParser: Parser[N], dimension: Dimension[T])(implicit num: Numeric[N]): Parser[T] =
    "(" ~> valueParser ~ "*" ~ unitOfMeasure(dimension) <~ ")" ^^ {
      case value ~ "*" ~ units ⇒ units(value)
    }

  def cosemValue[T](valueParser: Parser[T]): Parser[T] = "(" ~> valueParser <~ ")"

  def cosemObject[T <: Quantity[T], N](
                   id: Parser[ObisRef],
                   valueParser: Parser[N],
                   dimension: Dimension[T])(implicit num: Numeric[N]): Parser[T] =
    id ~> cosemValue(valueParser, dimension)

  def cosemObject[T](id: Parser[ObisRef], valueParser: Parser[T]): Parser[T] =
    id ~> cosemValue(valueParser)

  def mbusCosemObject[T](id: Parser[Int], valueParser: Parser[T]): Parser[(Int, T)] =
    id ~ cosemValue(valueParser) ^^ { case id ~ value ⇒ (id, value ) }

  def mbusValueTimestamp: Parser[DateTime] = "(" ~> timestamp <~ ")"

  def mbusCosemObjectValue[T](id: Parser[Int]):
      Parser[(Int, DateTime, BigDecimal, String)] =
    (id ~ mbusValueTimestamp ~ cosemValue(floatingPoint() ~ "*" ~ variableString(1, 3))) ^^ {
      case id ~ timestamp ~ (value ~ "*" ~ unitsOfMeasure) ⇒ (id, timestamp, value, unitsOfMeasure)
    }

  /**
    This seems to be incorrectly specified. The smart reader produces the sequence:
    id(count)(id2)(timestamp)(b1)(b2)(...) but specified is:
    id(timestamp)(count)(id2)(b1)(b2)(...)
    */
  def powerFailureLog[T](id: Parser[ObisRef], id2: Parser[ObisRef], bufferValueParser: Parser[T]): Parser[List[(DateTime, T)]] =
    id ~>
      cosemValue(decimals) ~>
      cosemValue(id2) ~>
      rep( (mbusValueTimestamp ~ bufferValueParser) ^^ {
        case tst ~ length ⇒ (tst, length)
      })

  def versionInformation: Parser[VersionInformation] =
    cosemObject((1,3,0,2,8), octetString(2, 2))
      .map(byteArray ⇒ VersionInformation(byteArray(0).toInt))

  def coTimestamp: Parser[DateTimeStamp] =
    cosemObject((0, 0, 1, 0, 0), timestamp).map(tst ⇒ DateTimeStamp(tst))

  def equipmentIdentifier: Parser[EquipmentIdentifier] =
    cosemObject((0, 0, 96, 1, 1), octetString(0, 96)).map(ba ⇒ EquipmentIdentifier(new String(ba)))

  def meterReading: Parser[MeterReading] = {
    def meterReadingParser(id: ObisRef, tariff: Tariff, direction: Direction): Parser[MeterReading] =
      cosemObject(id, floatingPoint(9, 3, 3), Energy).map { energy ⇒
        MeterReading(energy, tariff, direction)
      }

    meterReadingParser((1, 0, 1, 8, 1), Tariff1, ToClient) |
    meterReadingParser((1, 0, 1, 8, 2), Tariff2, ToClient) |
    meterReadingParser((1, 0, 2, 8, 1), Tariff1, FromClient) |
    meterReadingParser((1, 0, 2, 8, 2), Tariff2, FromClient)
  }

  def tariffIndicator: Parser[TariffIndicator] =
    cosemObject((0, 0, 96, 14, 0), integer(4)) ^^ {
      case 1 ⇒ TariffIndicator(Tariff1)
      case 2 ⇒ TariffIndicator(Tariff2)
    }

  def currentPower: Parser[CurrentPower] = {
    def currentPowerParser(id: ObisRef, direction: Direction): Parser[CurrentPower] =
      cosemObject(id, floatingPoint(5, 3, 3), Power) map { CurrentPower(_, direction) }

    currentPowerParser((1, 0, 1, 7, 0), ToClient) |
    currentPowerParser((1, 0, 2, 7, 0), FromClient)
  }

  def thresholdElectricity: Parser[ThresholdElectricity] =
    cosemObject( (0, 0, 17, 0, 0), floatingPoint(4, 1, 1), Power) map ThresholdElectricity

  def switchPositionElectricity: Parser[SwitchPositionElectricity] =
    cosemObject( (0, 0, 96, 3, 10), integer(1)) ^^ {
      case 0 ⇒ SwitchPositionElectricity(In)
      case 1 ⇒ SwitchPositionElectricity(Out)
      case 2 ⇒ SwitchPositionElectricity(Enabled)
    }

  def numberOfPowerFailures: Parser[NumberOfPowerFailures] =
    cosemObject( (0, 0, 96, 7, 21), integer(5)) map NumberOfPowerFailures

  def numberOfLongPowerFailures: Parser[NumberOfLongPowerFailures] =
    cosemObject( (0, 0, 96, 7, 9), integer(5)) map NumberOfLongPowerFailures

  def powerFailureEventLog: Parser[PowerFailureEventLog] =
    powerFailureLog( (1, 0, 99, 97, 0), (0, 0, 96, 7, 19), cosemValue(integer(10), Time)) ^^ PowerFailureEventLog

  def voltageAnomalies: Parser[VoltageAnomaly] = {
    def voltageAnomalyParser(id: ObisRef, phase: Phase, anomaly: AnomalyType): Parser[VoltageAnomaly] =
      cosemObject(id, integer(5)) map (VoltageAnomaly(_, phase, anomaly))

    voltageAnomalyParser((1, 0, 32, 32, 0), L1, Sag) |
      voltageAnomalyParser((1, 0, 52, 32, 0), L2, Sag) |
      voltageAnomalyParser((1, 0, 72, 32, 0), L3, Sag) |
      voltageAnomalyParser((1, 0, 32, 36, 0), L1, Swell) |
      voltageAnomalyParser((1, 0, 52, 36, 0), L2, Swell) |
      voltageAnomalyParser((1, 0, 72, 36, 0), L3, Swell)
  }

  def textMessageCodes: Parser[TextMessageCodes] =
    cosemObject((0, 0, 96, 13, 1), octetString(0, 16)) map (TextMessageCodes)

  def textMessage: Parser[TextMessage] =
    cosemObject((0, 0, 96, 13, 0), octetString(0, 2048) map (new String(_)))
      .map(TextMessage)

  def mbusDeviceType: Parser[MbusDeviceType] =
    mbusCosemObject((0, 24, 1, 0), integer(3)) map MbusDeviceType.tupled

  def mbusEquipmentIdentifier: Parser[MbusEquipmentIdentifier] =
    mbusCosemObject((0, 96, 1, 0), octetStringAsString(0, 96))
      .map(MbusEquipmentIdentifier.tupled)

  def mbusUnitsDeliveredToClient: Parser[MbusUnitsDeliveredToClient] =
    mbusCosemObjectValue((0, 24, 2, 1)).map(MbusUnitsDeliveredToClient.tupled)

  def valvePositionToEnumParser: Parser[ValvePosition] =
    integer(1) ^^ {
      case 0 ⇒ On
      case 1 ⇒ Off
      case 2 ⇒ Released
    }

  def instantaneousCurrent: Parser[InstantaneousCurrent] = {
    def instantaneousCurrentParser(id: ObisRef, phase: Phase): Parser[InstantaneousCurrent] =
      cosemObject(id, integer(3), ElectricCurrent) map (InstantaneousCurrent(phase, _))

    instantaneousCurrentParser( (1, 0, 31, 7, 0), L1) |
    instantaneousCurrentParser( (1, 0, 51, 7, 0), L2) |
    instantaneousCurrentParser( (1, 0, 71, 7, 0), L3)
  }

  def instantaneousPower: Parser[InstantaneousPower] = {
    def instantaneousPowerParser(id: ObisRef, phase: Phase, direction: Direction): Parser[InstantaneousPower] =
      cosemObject(id, floatingPoint(5, 3, 3), Power) map (InstantaneousPower(phase, _, direction))

    instantaneousPowerParser( (1, 0, 21, 7, 0), L1, ToClient) |
    instantaneousPowerParser( (1, 0, 41, 7, 0), L2, ToClient) |
    instantaneousPowerParser( (1, 0, 61, 7, 0), L3, ToClient) |
    instantaneousPowerParser( (1, 0, 22, 7, 0), L1, FromClient) |
    instantaneousPowerParser( (1, 0, 42, 7, 0), L2, FromClient) |
    instantaneousPowerParser( (1, 0, 62, 7, 0), L3, FromClient)
  }

  def mbusValvePosition: Parser[MbusValvePosition] =
    mbusCosemObject((0, 24, 4, 0), valvePositionToEnumParser) map MbusValvePosition.tupled

  def p1CosemObject: Parser[CosemObject] =
    versionInformation | coTimestamp | equipmentIdentifier |
      meterReading | tariffIndicator | currentPower | thresholdElectricity |
      switchPositionElectricity | numberOfPowerFailures | numberOfLongPowerFailures |
      powerFailureEventLog | voltageAnomalies | textMessageCodes | textMessage |
      mbusDeviceType | mbusEquipmentIdentifier | mbusUnitsDeliveredToClient |
      mbusValvePosition | instantaneousCurrent | instantaneousPower

  def crc: Parser[String] = hexadecimalValue(4, 4)

  def header: Parser[String] = "/" ~> stringValue(3) ~> "5" ~> variableString(1, 30)

  def footer: Parser[String] = "!" ~> crc

  def p1Telegram: Parser[P1Telegram] =
    header ~ nl ~ nl ~ rep(p1CosemObject <~ nl) <~ footer ^^ {
      case str ~ a ~ b ~ cosemObjects ⇒ P1Telegram(str, cosemObjects)
    }
}

object P1Parser extends P1Parser {

  def parseCosemObject(s: CharSequence): CosemObject =
    parseCosemObject(new CharSequenceReader(s))

  def parseDataMessage(s: CharSequence): P1Telegram =
    parseP1Telegram(new CharSequenceReader(s))

  def parseP1TelegramPhrase(input: CharSequenceReader): ParseResult[P1Telegram] =
    phrase(p1Telegram)(input)

  def parseCosemObjectPhrase(input: CharSequenceReader): ParseResult[CosemObject] =
    phrase(p1CosemObject)(input)

  def parseP1Telegram(input: CharSequenceReader): P1Telegram =
    parseP1TelegramPhrase(input) match {
      case Success(t, _) => t
      case NoSuccess(msg, next) => throw new IllegalArgumentException(
        "Could not parse '" + input + "' near '" + next.pos.longString + ": " + msg)
    }

  def parseCosemObject(input: CharSequenceReader): CosemObject =
    parseCosemObjectPhrase(input) match {
      case Success(t, _) => t
      case NoSuccess(msg, next) => throw new IllegalArgumentException(
        "Could not parse '" + input + "' near '" + next.pos.longString + ": " + msg)
    }
}


