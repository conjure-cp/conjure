import json

def inrc_to_essence(scenarioFile, historyFile, weekFile, essenceFile):
    with open(scenarioFile, 'rt') as f:
        scenarioData = json.load(f)

    with open(historyFile, 'rt') as f:
        historyData = json.load(f)

    with open(weekFile, 'rt') as f:
        weekData = json.load(f)
    
    lsLines = []

    # number of nurses
    nNurses = len(scenarioData['nurses'])
    lsLines.append('letting nNurses be ' + str(nNurses))

    # number of days
    nDays = 7
    lsLines.append('letting nDays be ' + str(nDays))

    # shifts 
    lsShifts = [s['id'] for s in scenarioData['shiftTypes']]
    lsLines.append('letting shifts be new type enum {' + ', '.join(lsShifts) + '}')

    # forbidden patterns
    lsForbiddenPatterns = []
    for p in scenarioData['forbiddenShiftTypeSuccessions']:
        for secondShift in p['succeedingShiftTypes']:
            lsForbiddenPatterns.append([p['precedingShiftType'],secondShift])
    s = 'letting forbiddenPatterns be {\n' \
        + ',\n'.join(['\t('+','.join(p)+')' for p in lsForbiddenPatterns]) \
        + '\n}'
    lsLines.append(s)

    # skill set
    lsSkills = scenarioData['skills']

    # day dict and index (starting from 0, must be incremented by 1)
    lsDayNames = ['Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday']
    dictDayIndex = {}
    for i in range(nDays):
        dictDayIndex[lsDayNames[i]] = i

    # minimum demand for each (day,shift)
    lsDayMinDemand = {}
    for d in range(nDays):
        for s in lsShifts:
            dayShiftName = str(d+1) + ',' + s
            lsDayMinDemand[dayShiftName] = 0
    for r in weekData['requirements']:
        shift = r['shiftType']
        for key, val in r.items():
            if 'requirementOn' in key:
                dayName = key.split('On')[1]
                dayShiftName = str(dictDayIndex[dayName]+1) + ',' + shift
                lsDayMinDemand[dayShiftName] += val['minimum']
    s = 'letting minimumDemand be function (\n'\
        + ',\n'.join(['\t(' + key + ') --> ' + str(val) for key, val in lsDayMinDemand.items()]) \
        + '\n)'
    lsLines.append(s)

    with open(essenceFile,'wt') as f:
        f.write('\n'.join(lsLines))


def test():
    dataDir = 'data/TestDatasets/perType/JSON/n005w4/'
    scenarioFile = dataDir + 'Sc-n005w4.json'
    historyFile = dataDir + 'H0-n005w4-0.json'
    weekFile = dataDir + 'WD-n005w4-0.json'
    essenceFile = './test-02.param'
    inrc_to_essence(scenarioFile, historyFile, weekFile, essenceFile)


test()

