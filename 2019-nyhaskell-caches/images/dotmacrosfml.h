#define POKEMON(ID, NAME, SPRITE, LEVEL, SPLIT, SHAPE) \
    ID[label=<<table border="0"> \
        <tr><td><img src=SPRITE/></td></tr> \
        <tr><td>NAME <b>LEVEL</b></td></tr> \
        <tr><td>Split <b>SPLIT</b></td></tr> \
        </table>>, shape=SHAPE];

#define MASK4(ID, B0, B1, B2, B3) \
    ID[label=<<table> \
        <tr><td>B0</td><td>B1</td><td>B2</td><td>B3</td></tr> \
        </table>>, shape=none];

#define BITS4(ID, NAME, SPRITE, LEVEL, B0, B1, B2, B3) \
    ID[label=<<table> \
        <tr><td colspan="4" border="0"><img src=SPRITE/></td></tr> \
        <tr><td colspan="4" border="0">NAME <b>LEVEL</b></td></tr> \
        <tr><td>B0</td><td>B1</td><td>B2</td><td>B3</td></tr> \
        </table>>, shape=none];

#define MASKBITS4(ID, M0, M1, M2, M3, NAME, SPRITE, LEVEL, B0, B1, B2, B3) \
    ID[label=<<table> \
        <tr><td>M0</td><td>M1</td><td>M2</td><td>M3</td></tr> \
        <tr><td colspan="4" border="0"><img src=SPRITE/></td></tr> \
        <tr><td colspan="4" border="0">NAME <b>LEVEL</b></td></tr> \
        <tr><td>B0</td><td>B1</td><td>B2</td><td>B3</td></tr> \
        </table>>, shape=none];
