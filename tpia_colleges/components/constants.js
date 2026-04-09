export const STATUS_META = {
  draft: { label: 'Draft', color: '#667085', bg: '#F4F7FB', description: 'Not yet sent' },
  sent: { label: 'Sent', color: '#155EEF', bg: '#EAF2FF', description: 'Awaiting response' },
  acknowledged: { label: 'Acknowledged', color: '#7A4CE0', bg: '#F3ECFF', description: 'Receipt confirmed' },
  in_progress: { label: 'In Progress', color: '#C2410C', bg: '#FFF2E5', description: 'Agency is processing' },
  fee_pending: { label: 'Fee Pending', color: '#B45309', bg: '#FFF6E8', description: 'Awaiting cost decision' },
  fee_paid: { label: 'Fee Paid', color: '#7C2D12', bg: '#FCEFE8', description: 'Payment sent' },
  ag_opinion_requested: { label: 'AG Opinion', color: '#B42318', bg: '#FDECEC', description: 'AG ruling requested' },
  ag_opinion_pending: { label: 'AG Pending', color: '#9F1239', bg: '#FBE7F0', description: 'Waiting on agency' },
  denied: { label: 'Denied', color: '#C0262D', bg: '#FDE8E8', description: 'Request refused' },
  partially_complete: { label: 'Partial', color: '#15803D', bg: '#EAF8EF', description: 'Some records received' },
  complete: { label: 'Complete', color: '#166534', bg: '#DCFCE7', description: 'All records received' },
  withdrawn: { label: 'Withdrawn', color: '#475569', bg: '#EAEFF5', description: 'Request withdrawn' },
};

export const STATUS_KEYS = Object.keys(STATUS_META);

export const TEXAS_COUNTIES = [
  'Anderson', 'Andrews', 'Angelina', 'Aransas', 'Archer', 'Armstrong', 'Atascosa', 'Austin', 'Bailey', 'Bandera', 'Bastrop', 'Baylor', 'Bee', 'Bell', 'Bexar', 'Blanco', 'Borden', 'Bosque', 'Bowie', 'Brazoria', 'Brazos', 'Brewster', 'Briscoe', 'Brooks', 'Brown', 'Burleson', 'Burnet', 'Caldwell', 'Calhoun', 'Callahan', 'Cameron', 'Camp', 'Carson', 'Cass', 'Castro', 'Chambers', 'Cherokee', 'Childress', 'Clay', 'Cochran', 'Coke', 'Coleman', 'Collin', 'Collingsworth', 'Colorado', 'Comal', 'Comanche', 'Concho', 'Cooke', 'Coryell', 'Cottle', 'Crane', 'Crockett', 'Crosby', 'Culberson', 'Dallam', 'Dallas', 'Dawson', 'Deaf Smith', 'Delta', 'Denton', 'DeWitt', 'Dickens', 'Dimmit', 'Donley', 'Duval', 'Eastland', 'Ector', 'Edwards', 'Ellis', 'El Paso', 'Erath', 'Erwin', 'Falls', 'Fannin', 'Fayette', 'Fisher', 'Floyd', 'Foard', 'Fort Bend', 'Franklin', 'Freestone', 'Frio', 'Gaines', 'Galveston', 'Garza', 'Gillespie', 'Glasscock', 'Goliad', 'Gonzales', 'Gray', 'Grayson', 'Gregg', 'Grimes', 'Guadalupe', 'Hale', 'Hall', 'Hamilton', 'Hansford', 'Hardeman', 'Hardin', 'Harris', 'Harrison', 'Hartley', 'Haskell', 'Hays', 'Hemphill', 'Henderson', 'Hidalgo', 'Hill', 'Hockley', 'Hood', 'Hopkins', 'Houston', 'Howard', 'Hudspeth', 'Hunt', 'Hutchinson', 'Irion', 'Jack', 'Jackson', 'Jasper', 'Jeff Davis', 'Jefferson', 'Jim Hogg', 'Jim Wells', 'Johnson', 'Jones', 'Karnes', 'Kaufman', 'Kendall', 'Kenedy', 'Kent', 'Kerr', 'Kimble', 'King', 'Kinney', 'Kleberg', 'Knox', 'La Salle', 'Lamar', 'Lamb', 'Lampasas', 'Latex', 'Lavaca', 'Lee', 'Leon', 'Liberty', 'Limestone', 'Lipscomb', 'Live Oak', 'Llano', 'Loving', 'Lubbock', 'Lynn', 'McCulloch', 'McLennan', 'McMullen', 'Madison', 'Marion', 'Martin', 'Mason', 'Matagorda', 'Maverick', 'Medina', 'Menard', 'Midland', 'Milam', 'Mills', 'Mitchell', 'Montague', 'Montgomery', 'Moore', 'Morris', 'Motley', 'Nacogdoches', 'Navarro', 'Newton', 'Nolan', 'Nueces', 'Ochiltree', 'Oldham', 'Orange', 'Palo Pinto', 'Panola', 'Parker', 'Parmer', 'Pecos', 'Polk', 'Potter', 'Presidio', 'Rains', 'Randall', 'Reagan', 'Real', 'Red River', 'Reeves', 'Refugio', 'Roberts', 'Robertson', 'Rockwall', 'Runnels', 'Rusk', 'Sabine', 'San Augustine', 'San Jacinto', 'San Patricio', 'San Saba', 'Schleicher', 'Scurry', 'Shackelford', 'Shelby', 'Sherman', 'Smith', 'Somervell', 'Starr', 'Stephens', 'Sterling', 'Stonewall', 'Sutton', 'Swisher', 'Tarrant', 'Taylor', 'Terrell', 'Terry', 'Throckmorton', 'Titus', 'Tom Green', 'Travis', 'Trinity', 'Tyler', 'Upshur', 'Upton', 'Uvalde', 'Val Verde', 'Van Zandt', 'Victoria', 'Walker', 'Waller', 'Ward', 'Washington', 'Webb', 'Wharton', 'Wheeler', 'Wichita', 'Wilbarger', 'Willacy', 'Williamson', 'Wilson', 'Winkler', 'Wise', 'Wood', 'Yoakum', 'Young', 'Zapata', 'Zavala',
];

export const SUGGESTED_TEMPLATE = {
  draft: '01_initial_request',
  sent: '02_follow_up',
  acknowledged: '02_follow_up',
  in_progress: '02_follow_up',
  fee_pending: '04_fee_payment',
  fee_paid: '04_fee_payment',
  ag_opinion_requested: '05_ag_opinion_response',
  ag_opinion_pending: '02_follow_up',
  denied: '06_denial_response',
  partially_complete: '07_records_received',
  complete: '07_records_received',
  withdrawn: '01_initial_request',
};

export const TEMPLATE_LABELS = {
  '01_initial_request': 'Initial request',
  '02_follow_up': 'Follow-up',
  '03_fee_waiver': 'Fee waiver response',
  '04_fee_payment': 'Fee payment response',
  '05_ag_opinion_response': 'AG opinion response',
  '06_denial_response': 'Denial response',
  '07_records_received': 'Records received',
};

export const SUBJECT_LINES = {
  '01_initial_request': 'Texas Public Information Act Request - Student Directory Information',
  '02_follow_up': 'Follow-Up: TPIA Request - {institution}',
  '03_fee_waiver': 'Re: Fee Waiver Request - TPIA Request - {institution}',
  '04_fee_payment': 'Re: Fee Estimate - TPIA Request - {institution}',
  '05_ag_opinion_response': 'Re: AG Opinion Notice - TPIA Request - {institution}',
  '06_denial_response': 'Re: Denial - TPIA Request - {institution}',
  '07_records_received': 'Confirmation: Records Received - {institution}',
};
