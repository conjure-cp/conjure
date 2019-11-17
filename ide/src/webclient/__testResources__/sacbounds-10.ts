export const core = {
  nodes: [
    {
      id: 0,
      parentId: -1,
      label: "",
      prettyLabel: "",
      childCount: 1,
      isSolution: false,
      isLeftChild: true,
      descCount: 64
    },
    {
      id: 1,
      parentId: 0,
      label: "Root Propagation",
      prettyLabel: "Root Propagation",
      childCount: 2,
      isSolution: false,
      isLeftChild: true,
      descCount: 63
    },
    {
      id: 32,
      parentId: 1,
      label: "setA_Occurrence_00001 != 0",
      prettyLabel: "setA_Occurrence_00001 != 0",
      childCount: 2,
      isSolution: false,
      isLeftChild: false,
      descCount: 32
    },
    {
      id: 51,
      parentId: 32,
      label: "setA_Occurrence_00002 != 0",
      prettyLabel: "setA_Occurrence_00002 != 0",
      childCount: 2,
      isSolution: false,
      isLeftChild: false,
      descCount: 13
    },
    {
      id: 61,
      parentId: 51,
      label: "setA_Occurrence_00003 != 0",
      prettyLabel: "setA_Occurrence_00003 != 0",
      childCount: 1,
      isSolution: false,
      isLeftChild: false,
      descCount: 3
    },
    {
      id: 62,
      parentId: 61,
      label: "setA_Occurrence_00004 = 0",
      prettyLabel: "setA_Occurrence_00004 = 0",
      childCount: 1,
      isSolution: false,
      isLeftChild: true,
      descCount: 2
    },
    {
      id: 63,
      parentId: 62,
      label: "setA_Occurrence_00005 = 0",
      prettyLabel: "setA_Occurrence_00005 = 0",
      childCount: 1,
      isSolution: false,
      isLeftChild: true,
      descCount: 1
    },
    {
      id: 64,
      parentId: 63,
      label: "setA_Occurrence_00006 = 0",
      prettyLabel: "setA_Occurrence_00006 = 0",
      childCount: 0,
      isSolution: false,
      isLeftChild: true,
      descCount: 0
    },
    {
      id: 2,
      parentId: 1,
      label: "setA_Occurrence_00001 = 0",
      prettyLabel: "setA_Occurrence_00001 = 0",
      childCount: 2,
      isSolution: false,
      isLeftChild: true,
      descCount: 29
    },
    {
      id: 33,
      parentId: 32,
      label: "setA_Occurrence_00002 = 0",
      prettyLabel: "setA_Occurrence_00002 = 0",
      childCount: 2,
      isSolution: false,
      isLeftChild: true,
      descCount: 17
    },
    {
      id: 52,
      parentId: 51,
      label: "setA_Occurrence_00003 = 0",
      prettyLabel: "setA_Occurrence_00003 = 0",
      childCount: 2,
      isSolution: false,
      isLeftChild: true,
      descCount: 8
    }
  ],
  solAncestorIds: [0, 1, 32, 51, 61, 62, 63, 64]
}
