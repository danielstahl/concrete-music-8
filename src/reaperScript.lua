
function GetChannel(name)
  for i = 0, reaper.GetNumAudioInputs() - 1 do
    local channelName = reaper.GetInputChannelName(i)
    if channelName == name then return i
    end
  end
  return -1
end  

function CreateFolder(index, name)
  reaper.InsertTrackAtIndex(index, false)
  folder = reaper.GetTrack(0, index)
  reaper.GetSetMediaTrackInfo_String(folder, 'P_NAME', name, true)
  reaper.SetMediaTrackInfo_Value( folder, 'I_FOLDERDEPTH',1)
  reaper.SetMediaTrackInfo_Value(folder, 'I_FOLDERCOMPACT', 0)
end

function CreateTrack(index, name, channel, lastInFolder)
  local ch = GetChannel(channel)
  local folderDepth = 0
  if lastInFolder then folderDepth = -1 end
  
  reaper.InsertTrackAtIndex(index, false)
  tr = reaper.GetTrack(0,index)
  reaper.GetSetMediaTrackInfo_String(tr, 'P_NAME', name, true)
  reaper.SetMediaTrackInfo_Value( tr, 'I_RECARM',1)
  reaper.SetMediaTrackInfo_Value( tr, 'I_RECINPUT',1024 + ch)
  reaper.SetMediaTrackInfo_Value( tr, 'I_FOLDERDEPTH',folderDepth)
end

CreateFolder(0, "Metal bird")
CreateTrack(1, "Metal Bird 1", "Input 1 (BlackHole 64ch)", false)
CreateTrack(2, "Metal Bird 2", "Input 3 (BlackHole 64ch)", false)
CreateTrack(3, "Metal Bird 3", "Input 5 (BlackHole 64ch)", true)
CreateTrack(3, "Metal Bird 3", "Input 7 (BlackHole 64ch)", true)


CreateFolder(4, "Wood bird")
CreateTrack(5, "Wood Bird 1", "Input 9 (BlackHole 64ch)", false)
CreateTrack(6, "Wood Bird 2", "Input 11 (BlackHole 64ch)", true)

CreateFolder(7, "Metal env")
CreateTrack(8, "Metal env 1", "Input 13 (BlackHole 64ch)", false)
CreateTrack(9, "Metal env 2", "Input 15 (BlackHole 64ch)", false)
CreateTrack(10, "Metal env 3", "Input 17 (BlackHole 64ch)", false)
CreateTrack(11, "Metal env 4", "Input 19 (BlackHole 64ch)", true)
CreateTrack(11, "Metal env 4", "Input 21 (BlackHole 64ch)", true)

CreateFolder(12, "Wood env")
CreateTrack(13, "Wood env 1", "Input 23 (BlackHole 64ch)", false)
CreateTrack(14, "Wood env 2", "Input 25 (BlackHole 64ch)", false)
CreateTrack(15, "Wood env 3", "Input 27 (BlackHole 64ch)", false)
CreateTrack(16, "Wood env 4", "Input 29 (BlackHole 64ch)", false)
CreateTrack(17, "Wood env 5", "Input 31 (BlackHole 64ch)", false)
CreateTrack(18, "Wood env 6", "Input 33 (BlackHole 64ch)", false)
CreateTrack(19, "Wood env 7", "Input 35 (BlackHole 64ch)", false)
CreateTrack(20, "Wood env 8", "Input 37 (BlackHole 64ch)", true)



